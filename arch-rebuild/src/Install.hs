{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Install where

import RIO hiding (threadDelay)
import RIO.Directory
import RIO.Process

import Data.String.Interpolate
import System.Exit (exitFailure)
import System.Posix.User (getEffectiveUserID)
import Time.Units (Second, Time(..), threadDelay)

import Command
import Disk
import Match

installArch :: (MonadIO m, MonadReader env m, HasLogFunc env) => FilePath -> m ()
installArch device = do
    InstallDiskInfo {..} <- getInstallDiskInfo device
    let cwd = "XXX" :: String -- XXX
    logInfo "Installing Arch"
    runCmds_
        [ [i|cp -v #{cwd}/mirrorlist /etc/pacman.d/|]
        , [i|pacstrap /mnt base btrfs-progs ${install_pkgs[@]} ${install_groups[@]}|]
        ]
    logInfo "Configuring chroot Arch"
    runCmd_ [i|#{cwd}/fstab.sh #{uuidEsp} #{uuidRootfs} >>/mnt/etc/fstab|]

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
    (not . null) . linesMatchingWords (["synchronized", "no"] :: [String]) <$>
        readProcessStdout_ "timedatectl status"
