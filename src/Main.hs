{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import RIO
import RIO.Process
import RIO.Text (pack, unpack)

import Options.Generic
import Text.Show.Pretty (pPrint)
import UnliftIO.Environment (getProgName)

import AUR
import Build
import Checks
import Chroot
import Config
import FsTree
import Version

data CmdOpts
  = WipeRootDisk {confPath :: FilePath}
  | BuildArch {confPath :: FilePath, etcPath :: Maybe FilePath, aurPkgsPath :: Maybe FilePath}
  | ConfigureRootfs {buildInfoPath :: FilePath}
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
  cmd <- getProgName >>= getRecord . pack
  let run =
        case cmd of
          WipeRootDisk confPath -> do
            sysConf <- temporarySystemConfig <$> loadSystemConfig confPath
            doPreInstallChecks sysConf
            wipeRootDisk sysConf
          BuildArch confPath etcPath aurPkgsPath -> do
            sysConf <- loadSystemConfig confPath
            doPreInstallChecks $ temporarySystemConfig sysConf
            buildArch sysConf etcPath aurPkgsPath
          ConfigureRootfs buildInfoPath -> do
            buildInfo <- loadBuildInfo buildInfoPath
            configureRootfs $ buildInfo ^. systemConfig
            customizeRootfs $ buildInfo ^. systemConfig . extraFiles
          BuildAurPackages confPath dest -> do
            sysConf <- temporarySystemConfig <$> loadSystemConfig confPath
            forM_ (unpack <$> sysConf ^. pacman . aur) (buildAURPackage dest)
          ShowBuildInfo buildInfoPath -> do
            buildInfo <- loadBuildInfo buildInfoPath
            liftIO $ pPrint buildInfo
          Version ->
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

customizeRootfs :: (MonadIO m, MonadReader env m, HasLogFunc env) => [FileType] -> m ()
customizeRootfs files = do
  logInfo "Copying extra files to rootfs"
  mapM_ copy files
  where
    parseAttrs (mode, owner) =
      let parse = fromMaybe (error "Main.customizeRootfs: invalid file attributes") . readMaybe
       in (parse . unpack <$> mode, bimap unpack unpack <$> owner)
    copy (Regular path content attrs) = createFsTree $ File path (Content content) $ parseAttrs attrs
    copy (Directory path attrs) = createFsTree $ Dir path (parseAttrs attrs) []
    copy (Config.WithAttrs path attrs) = createFsTree $ FsTree.WithAttrs path $ parseAttrs attrs
