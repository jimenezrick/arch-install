{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Install where

import RIO hiding (threadDelay)
import RIO.Directory
import RIO.Process

import Control.Monad.Except
import Data.String.Interpolate
import Data.Text.IO (writeFile)
import System.Exit (exitFailure)
import System.Posix.User (getEffectiveUserID)
import Time.Units (Second, Time(..), threadDelay)

import Command
import Config
import Fstab
import Match

installArch :: (MonadIO m, MonadReader env m, HasLogFunc env) => SystemConfig -> ExceptT String m ()
installArch sysConf = do
    logInfo "Installing Arch"
    liftIO $ writeFile "/etc/pacman.d/mirrorlist" $ sysConf ^. pacmanMirrorlist
    runCmd_
        [i|pacstrap /mnt base btrfs-progs #{sysConf^.pacmanExplicitPackages} #{sysConf^.pacmanPackageGroups}|]
    logInfo "Configuring Arch chroot"
    fstab <- renderFstab $ sysConf ^. fstabEntries
    liftIO $ writeFile "/mnt/etc/fstab" fstab

doPreInstallChecks :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env) => m ()
doPreInstallChecks = do
    amIRoot >>= exitIfNot (logError "must be run as root")
    isUefiSystem >>= exitIfNot (logError "not booted in UEFI mode")
    isNetworkReady >>= exitIfNot (logError "network not ready")
    isClockSynced >>= exitIfNot (logError "clock not in sync")
  where
    exitIfNot f b = unless b f >> void (liftIO exitFailure)

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