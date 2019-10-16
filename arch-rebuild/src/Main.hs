{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import RIO
import RIO.FilePath ((</>))
import RIO.Process
import RIO.Text (pack)

import Options.Generic
import UnliftIO.Environment (getProgName)

import Checks
import Chroot
import Config
import Install

data CmdOpts
    = BuildRootfs { confPath :: FilePath }
    | ConfigureChroot { binConfPath :: FilePath }
    | InstallBootloaderChroot { binConfPath :: FilePath }
    | CopyDiskImages { confPath :: FilePath }
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
            case cmd of
                BuildRootfs confPath -> do
                    sysConf <- loadSystemConfig $ confPath </> "system.dhall"
                    buildRootfs sysConf
                ConfigureChroot binConfPath -> do
                    sysConf <- loadBinSystemConfig binConfPath
                    configureArch sysConf
                InstallBootloaderChroot binConfPath -> do
                    sysConf <- loadBinSystemConfig binConfPath
                    installBootloader sysConf
                CopyDiskImages confPath
                    -- TODO: check target disk isn't mounted
                 -> do
                    sysConf <- loadSystemConfig $ confPath </> "system.dhall"
                    copyDiskRootfsImage confPath
    runApp $ do
        doPreInstallChecks
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
