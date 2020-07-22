{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import RIO
import RIO.Directory (listDirectory)
import RIO.FilePath
import RIO.Process
import RIO.Text (pack, unpack)
import RIO.List (isInfixOf)

import Data.String.Interpolate
import Options.Generic
import Text.Show.Pretty (pPrint)
import UnliftIO.Environment (getProgName)

import AUR
import Build
import Checks
import Chroot
import Command
import Config
import FsTree
import Version

data CmdOpts
  = WipeRootDisk {confPath :: FilePath}
  | BuildArch {confPath :: FilePath, aurPkgsPath :: Maybe FilePath}
  | ConfigureRootfs {buildInfoPath :: FilePath}
  | RestoreEtc {etcSrc :: FilePath}
  | BuildAurPackages {confPath :: FilePath, dest :: FilePath}
  | ShowBuildInfo {buildInfoPath :: FilePath}
  | Version
  deriving (Generic)

instance ParseRecord CmdOpts where
  parseRecord = parseRecordWithModifiers $ lispCaseModifiers {shortNameModifier = firstLetter}

data App = App
  { saLogFunc :: !LogFunc,
    saProcessContext :: !ProcessContext
  }

instance HasLogFunc App where
  logFuncL = lens saLogFunc (\x y -> x {saLogFunc = y})

instance HasProcessContext App where
  processContextL = lens saProcessContext (\x y -> x {saProcessContext = y})

main :: IO ()
main = do
  cmd <- pack <$> getProgName >>= getRecord
  let run =
        case cmd of
          WipeRootDisk confPath -> do
            sysConf <- temporarySystemConfig <$> loadSystemConfig confPath
            doPreInstallChecks sysConf
            wipeRootDisk sysConf
          BuildArch confPath aurPkgsPath -> do
            sysConf <- loadSystemConfig confPath
            doPreInstallChecks $ temporarySystemConfig sysConf
            buildArch sysConf
            case aurPkgsPath of
              Nothing -> return ()
              Just dir -> do
                logInfo "Installing pre-built AUR packages"
                pkgs <- map (dir </>) . filter (isInfixOf ".pkg.tar.") <$> listDirectory dir
                forM_ pkgs installAURPackage
            umountRootfs
          ConfigureRootfs buildInfoPath -> do
            buildInfo <- loadBuildInfo buildInfoPath
            configureRootfs $ buildInfo ^. systemConfig
            customizeRootfs $ buildInfo ^. systemConfig . custom
          RestoreEtc etcSrc -> do
            runCmds_ ["rm -r /etc", [i|git clone #{etcSrc} /etc|], "chmod 700 /etc/.git"]
            logInfo "NOTE: you need to update /etc/fstab and restore file permissions" -- XXX: use mtree directly with arch-chroot?
          BuildAurPackages confPath dest -> do
            sysConf <- temporarySystemConfig <$> loadSystemConfig confPath
            forM_ (unpack <$> sysConf ^. pacman . aur) (buildAURPackage dest)
          ShowBuildInfo buildInfoPath -> do
            buildInfo <- loadBuildInfo buildInfoPath
            liftIO $ pPrint buildInfo
          Version -> do
            liftIO $ pPrint version
  runApp $ do
    catch run (\(ex :: SomeException) -> logError (displayShow ex) >> liftIO exitFailure)
    logInfo "Done"

runApp :: MonadIO m => RIO App a -> m a
runApp m =
  liftIO $ do
    lo <- logOptionsHandle stderr True
    pc <- mkDefaultProcessContext
    withLogFunc lo $ \lf ->
      let simpleApp = App {saLogFunc = lf, saProcessContext = pc}
       in runRIO simpleApp m

-- TODO: Have secrets.dhall?
customizeRootfs ::
  (MonadIO m, MonadReader env m, HasLogFunc env) => Maybe [(FilePath, Text)] -> m ()
customizeRootfs custom = do
  logInfo "Customizing rootfs to my liking"
  createFsTree $
    Dir
      "/mnt"
      defAttrs
      [Dir "scratch" defAttrs [], Dir "garage" defAttrs [], Dir "usb" defAttrs []]
  maybe
    (return ())
    ( mapM_ $ \(path, content) -> do
        logInfo $ fromString [i|Creating file: #{path}|]
        createFile path content
    )
    custom
  where
    createFile path content
      | "/var/lib/iwd" <- takeDirectory path =
        createFsTree $
          Dir
            "/var/lib/iwd"
            (Just 0o700, Just ("root", "root"))
            [File (takeFileName path) (Content content) (Just 0o600, Just ("root", "root"))]
      | otherwise = createFsTree $ File path (Content content) defAttrs
