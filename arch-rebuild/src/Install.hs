{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Install where

import RIO hiding (threadDelay, unwords)
import RIO.Directory

import Data.String.Interpolate
import Data.Text (unwords)
import Data.Text.IO (writeFile)

import Command
import Config
import Filesystem
import Fstab

buildRootfs :: (MonadIO m, MonadReader env m, HasLogFunc env) => InstallConfig -> m ()
buildRootfs installConf = do
    createImgs
    formatImgs
    mountImgs
    bootstrapArch
    umountImgs
  where
    espMnt = "/mnt/esp"
    rootfsMnt = "/mnt/rootfs"
    espPath = installConf ^. espImage
    rootfsPath = installConf ^. rootfsImage
    createImgs = do
        logInfo $ fromString [i|Creating ESP image: #{espPath}|]
        createZeroImage espPath 1
        logInfo $ fromString [i|Creating rootfs image: #{rootfsPath}|]
        createZeroImage rootfsPath 20
    formatImgs = do
        logInfo $ fromString [i|Formatting ESP: #{espPath}|]
        runCmd_ [i|mkfs.fat -F32 #{espPath}|]
        logInfo $ fromString [i|Formatting rootfs partition: #{rootfsPath}|]
        runCmd_ [i|mkfs.btrfs #{rootfsPath}|]
    mountImgs = do
        createDirectoryIfMissing False espMnt
        createDirectoryIfMissing False rootfsMnt
        logInfo $ fromString [i|Mounting ESP on: #{espMnt}|]
        mountLoopImage espPath espMnt
        logInfo $ fromString [i|Mounting rootfs on: #{rootfsMnt}|]
        mountLoopImage rootfsPath rootfsMnt
    bootstrapArch = do
        logInfo $ fromString [i|Bootstrapping Arch on: #{rootfsMnt}|]
        liftIO $ writeFile "/etc/pacman.d/mirrorlist" $ installConf ^. system . pacman . mirrorlist
        let packages = unwords $ installConf ^. system . pacman . explicitPackages
            groups = unwords $ installConf ^. system . pacman . packageGroups
        runCmd_ [i|pacstrap #{rootfsMnt} #{packages} #{groups}|]
    umountImgs = do
        umountPoint espMnt
        umountPoint rootfsMnt

partitionDisk :: (MonadIO m, MonadReader env m, HasLogFunc env) => BlockDev -> m ()
partitionDisk (DevPath path) = do
    logInfo $ fromString [i|Partitioning device: #{path}|]
    runCmd_ $ [i|parted -s #{path} -a optimal|] ++
        " mklabel gpt \
        \ mkpart primary 0% 513MiB \
        \ mkpart primary 513MiB 100% \
        \ set 1 boot on"
    logInfo $ fromString [i|Formatting ESP: #{espPath}|]
    runCmd_ [i|mkfs.fat -F32 #{espPath}|]
    logInfo $ fromString [i|Formatting rootfs partition: #{rootfsPath}|]
    runCmd_ [i|mkfs.btrfs #{rootfsPath}|]
  where
    espPath = [i|#{path}/1|]
    rootfsPath = [i|#{path}/2|]
partitionDisk dev = throwString [i|unsopported block device: #{dev}|]

installArch :: (MonadIO m, MonadReader env m, HasLogFunc env) => SystemConfig -> m ()
installArch sysConf = do
    logInfo "Installing Arch"
    liftIO $ writeFile "/etc/pacman.d/mirrorlist" $ sysConf ^. pacman . mirrorlist
    runCmd_
        [i|pacstrap /mnt base btrfs-progs #{sysConf^.pacman.explicitPackages} #{sysConf^.pacman.packageGroups}|]
    logInfo "Configuring Arch chroot"
    fstab <- renderFstab $ sysConf ^. fstabEntries
    liftIO $ writeFile "/mnt/etc/fstab" fstab
