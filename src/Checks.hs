{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Checks where

import RIO
import RIO.Directory
import RIO.Process

import System.Posix.User (getEffectiveUserID)
import Data.Time.Units

import Config
import Disk
import Match

exitIfNot :: MonadIO m => m a -> Bool -> m ()
exitIfNot f b = unless b (f >> void (liftIO exitFailure))

doPreInstallChecks :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env) => SystemConfig -> m ()
doPreInstallChecks sysConf = do
    logInfo "Doing pre-install checks"
    isRootDiskInQemu sysConf >>= exitIfNot (logError "Target root disk is not a QEMU device")
    isRootDiskNotMounted sysConf >>= exitIfNot (logError "Target root disk is currently mounted")
    amIRoot >>= exitIfNot (logError "Must be run as root")
    isUefiSystem >>= exitIfNot (logError "Not booted in UEFI mode")
    isNetworkReady >>= exitIfNot (logError "Network not ready")
    isClockSynced >>= exitIfNot (logError "Clock not in sync")

isRootDiskInQemu :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env) => SystemConfig -> m Bool
isRootDiskInQemu sysConf = do
    rootDiskInfo <- getDiskInfo $ sysConf ^. storage . rootDisk
    return $
        case rootDiskInfo of
            DiskInfo {Disk.model = Just "QEMU_HARDDISK"} -> True
            DiskWithPartitionsInfo {Disk.model = Just "QEMU_HARDDISK"} -> True
            _ -> False

isRootDiskNotMounted ::
       (MonadUnliftIO m, MonadReader env m, HasLogFunc env) => SystemConfig -> m Bool
isRootDiskNotMounted sysConf = not <$> isDiskMounted (sysConf ^. storage . rootDisk)

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
    threadDelay . fromIntegral $ toMicroseconds @Second 4
    (1 ==) . length . linesMatchingExactWords (["synchronized:", "yes"] :: [String]) <$>
        readProcessStdout_ "timedatectl status"
