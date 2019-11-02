{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import RIO
import RIO.Process
import RIO.Text (pack)

import Options.Generic
import Text.Show.Pretty (pPrint)
import UnliftIO.Environment (getProgName)

import Checks
import Chroot
import Config
import FsTree
import Install

data CmdOpts
    = WipeRootDisk { confPath :: FilePath }
    | BuildArch { confPath :: FilePath }
    | ConfigureRootfs { buildInfoPath :: FilePath }
    | ShowBuildInfo { buildInfoPath :: FilePath }
    deriving (Generic)

instance ParseRecord CmdOpts where
    parseRecord = parseRecordWithModifiers $ lispCaseModifiers {shortNameModifier = firstLetter}

data App = App
    { saLogFunc :: !LogFunc
    , saProcessContext :: !ProcessContext
    }

instance HasLogFunc App where
    logFuncL = lens saLogFunc (\x y -> x {saLogFunc = y})

instance HasProcessContext App where
    processContextL = lens saProcessContext (\x y -> x {saProcessContext = y})

main :: IO ()
main = do
    cmd <- pack <$> getProgName >>= getRecord
    let run =
            case cmd
                -- TODO: fetch /etc + /home BTRFS subvols
                  of
                WipeRootDisk confPath -> do
                    sysConf <- temporarySystemConfig <$> loadSystemConfig confPath
                    doPreInstallChecks sysConf
                    wipeRootDisk sysConf
                BuildArch confPath -> do
                    sysConf <- loadSystemConfig confPath
                    doPreInstallChecks $ temporarySystemConfig sysConf
                    buildArch sysConf
                ConfigureRootfs buildInfoPath -> do
                    buildInfo <- loadBuildInfo buildInfoPath
                    configureRootfs $ buildInfo ^. systemConfig
                    customizeRootfs
                ShowBuildInfo buildInfoPath -> do
                    buildInfo <- loadBuildInfo buildInfoPath
                    liftIO $ pPrint buildInfo
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

customizeRootfs :: (MonadIO m, MonadReader env m, HasLogFunc env) => m ()
customizeRootfs = do
    logInfo "Customizing rootfs"
    createFsTree $
        Dir
            "/mnt"
            defAttrs
            [Dir "scratch" defAttrs [], Dir "garage" defAttrs [], Dir "usb" defAttrs []]
