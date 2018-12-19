{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import RIO hiding (threadDelay)
import RIO.Directory
import RIO.Process

import Data.String.Conversions (cs)
import Data.String.Interpolate
import Labels
import System.Exit (exitFailure)
import System.Posix.User (getEffectiveUserID)
import Time.Units (Second, Time(..), threadDelay)

import Command
import Match

main :: IO ()
main = runSimpleApp (doPreInstallChecks >> installArch device)
  where
    device = "/dev/sda" -- XXX

doPreInstallChecks :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env) => m ()
doPreInstallChecks = do
    amIRoot >>= exitIfNot (logError "must be run as root")
    isUefiSystem >>= exitIfNot (logError "not booted in UEFI mode")
    isNetworkReady >>= exitIfNot (logError "network not ready")
    isClockSynced >>= exitIfNot (logError "clock not in sync")
  where
    exitIfNot f b = unless b f >> void (liftIO exitFailure)

--
-- Utilities
--
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
    (not . null) . linesMatchingWords ["synchronized", "no"] . cs <$>
        readProcessStdout_ "timedatectl status"

--
-- Installer
--
installArch :: (MonadIO m, MonadReader env m, HasLogFunc env) => Text -> m ()
installArch device = do
    devs <- pickInstallDevices device
    let cwd = "XXX" -- XXX
    logInfo "Installing Arch"
    runCommands_
        [ [i|cp -v #{cwd}/mirrorlist /etc/pacman.d/|]
        , [i|pacstrap /mnt base btrfs-progs ${install_pkgs[@]} ${install_groups[@]}|]
        ]
    logInfo "Configuring chroot Arch"
    runCommand_ [i|#{cwd}/fstab.sh #{espUuid devs} #{rootfsUuid devs} >>/mnt/etc/fstab|]

data InstallDevices = InstallDevices
    { devEsp :: Text
    , devRootfs :: Text
    , espUuid :: Text
    , rootfsUuid :: Text
    }

pickInstallDevices :: MonadIO m => Text -> m InstallDevices
pickInstallDevices device = do
    let devEsp = fromString [i|#{device}1|]
        devRootfs = fromString [i|#{device}2|]
    espUuid <- readCommandStdoutOneLine_ [i|lsblk -n -o UUID #{devEsp}|]
    rootfsUuid <- readCommandStdoutOneLine_ [i|lsblk -n -o UUID #{devRootfs}|]
    return InstallDevices {..}

--
-- XXX: Experiment
--
experiment :: Has "foo" value record => record -> value
experiment = get #foo
