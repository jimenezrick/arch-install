{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Checks where

import RIO hiding (threadDelay)
import RIO.Directory
import RIO.Process

import System.Posix.User (getEffectiveUserID)
import Time.Units (Second, Time(..), threadDelay)

import Config
import Disk
import Match

exitIfNot :: MonadIO m => m a -> Bool -> m ()
exitIfNot f b = unless b (f >> void (liftIO exitFailure))

doPreCopyChecks :: (MonadIO m, MonadReader env m, HasLogFunc env) => SystemConfig -> m ()
doPreCopyChecks sysConf = do
    logInfo "Doing pre-copy checks"
    rootDiskDev <- findDiskDevice $ sysConf ^. storage . rootDiskModel
    not <$> isDiskMounted rootDiskDev >>=
        exitIfNot (logError "target root disk is currently mounted")

doPreInstallChecks :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env) => m ()
doPreInstallChecks = do
    logInfo "Doing pre-install checks"
    amIRoot >>= exitIfNot (logError "must be run as root")
    isUefiSystem >>= exitIfNot (logError "not booted in UEFI mode")
    isNetworkReady >>= exitIfNot (logError "network not ready")
    isClockSynced >>= exitIfNot (logError "clock not in sync")

amIRoot :: MonadIO m => m Bool
amIRoot = (== 0) <$> liftIO getEffectiveUserID

isUefiSystem :: MonadIO m => m Bool
isUefiSystem = doesDirectoryExist "/sys/firmware/efi"

isNetworkReady :: MonadUnliftIO m => m Bool
isNetworkReady =
    catch
        (readProcess "ping -c1 archlinux.org" >> return True)
        (\(_ :: ExitCodeException) -> return False)

isClockSynced :: MonadIO m => m Bool
isClockSynced = do
    runProcess_ "timedatectl set-ntp true"
    threadDelay (Time @Second 2)
    (1 ==) . length . linesMatchingExactWords (["synchronized:", "yes"] :: [String]) <$>
        readProcessStdout_ "timedatectl status"
