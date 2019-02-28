{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Checks where

import RIO hiding (threadDelay)
import RIO.Directory
import RIO.Process

import System.Exit (exitFailure)
import System.Posix.User (getEffectiveUserID)
import Time.Units (Second, Time(..), threadDelay)

import Match

doPreInstallChecks :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env) => m ()
doPreInstallChecks = do
    amIRoot >>= exitIfNot (logError "must be run as root")
    isUefiSystem >>= exitIfNot (logError "not booted in UEFI mode")
    isNetworkReady >>= exitIfNot (logError "network not ready")
    isClockSynced >>= exitIfNot (logError "clock not in sync")
  where
    exitIfNot f b = unless b (f >> void (liftIO exitFailure))

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
