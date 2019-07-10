{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Install where

import RIO hiding (threadDelay, unwords)
import RIO.Directory
import RIO.FilePath ((</>), takeDirectory)

import Data.String.Interpolate
import Data.Text (unwords)
import Data.Text.IO (writeFile)

import Command
import Config
import Disk
import Filesystem
import Fstab

buildRootfs :: (MonadIO m, MonadReader env m, HasLogFunc env) => InstallConfig -> m ()
buildRootfs installConf = do
    createImgs
    formatImgs
    mountImgs
    -- TODO: create btrfs subvols
    bootstrapArch
    -- TODO: arch-chroot run settings
    -- TODO: prepare bootloader
    personalCustomization
    umountImgs
  where
    espPath = installConf ^. espImage
    rootfsPath = installConf ^. rootfsImage
    rootfsMnt = takeDirectory rootfsPath </> "rootfs"
    espMnt = rootfsMnt </> "boot"
    createImgs = do
        logInfo $ fromString [i|Creating ESP image: #{espPath}|]
        createZeroImage espPath $ installConf ^. espImageSize
        logInfo $ fromString [i|Creating rootfs image: #{rootfsPath}|]
        createZeroImage rootfsPath $ installConf ^. rootfsImageSize
    formatImgs = do
        logInfo $ fromString [i|Formatting ESP: #{espPath}|]
        runCmd_ [i|mkfs.fat -F32 #{espPath}|]
        logInfo $ fromString [i|Formatting rootfs partition: #{rootfsPath}|]
        runCmd_ [i|mkfs.btrfs #{rootfsPath}|]
    mountImgs = do
        createDirectoryIfMissing False rootfsMnt
        logInfo $ fromString [i|Mounting rootfs on: #{rootfsMnt}|]
        mountLoopImage rootfsPath rootfsMnt
        createDirectoryIfMissing False espMnt
        logInfo $ fromString [i|Mounting ESP on: #{espMnt}|]
        mountLoopImage espPath espMnt
    bootstrapArch = do
        logInfo $ fromString [i|Bootstrapping Arch on: #{rootfsMnt}|]
        let packages = unwords $ installConf ^. system . pacman . explicitPackages
            groups = unwords $ installConf ^. system . pacman . packageGroups
        runCmd_ [i|pacstrap #{rootfsMnt} #{packages} #{groups}|]
        let mirrorlistPath = rootfsMnt </> "etc/pacman.d/mirrorlist"
        logInfo $ fromString [i|Copying mirrorlist to: #{mirrorlistPath}|]
        liftIO $ writeFile mirrorlistPath $ installConf ^. system . pacman . mirrorlist
        let fstabPath = rootfsMnt </> "etc/fstab"
        logInfo $ fromString [i|Rendering fstab to: #{fstabPath}|]
        liftIO $ writeFile fstabPath =<< renderFstab (installConf ^. system . fstabEntries)
    personalCustomization = do
        logInfo $ fromString [i|Customizing rootfs on: #{rootfsMnt}|]
        createDirectoryIfMissing True $ rootfsMnt </> "mnt/scratch"
        createDirectoryIfMissing True $ rootfsMnt </> "mnt/garage"
        createDirectoryIfMissing True $ rootfsMnt </> "mnt/usb"
    umountImgs = do
        umountPoint espMnt
        umountPoint rootfsMnt

partitionDisk :: (MonadIO m, MonadReader env m, HasLogFunc env) => BlockDev -> m ()
partitionDisk blockdev = do
    dev <- findDevice
    let espDev = [i|#{dev}/1|]
        rootfsDev = [i|#{dev}/2|]
    logInfo $ fromString [i|Partitioning device: #{dev}|]
    runCmd_ $ [i|parted -s #{dev} -a optimal|] ++
        " mklabel gpt \
        \ mkpart primary 0% 513MiB \
        \ mkpart primary 513MiB 100% \
        \ set 1 boot on"
    logInfo $ fromString [i|Formatting ESP: #{espDev}|]
    runCmd_ [i|mkfs.fat -F32 #{espDev}|]
    logInfo $ fromString [i|Formatting rootfs partition: #{rootfsDev}|]
    runCmd_ [i|mkfs.btrfs #{rootfsDev}|]
    --
    -- TODO: btrfs subvols, in a different function
    --
  where
    findDevice =
        case blockdev of
            (DevPath path) -> return path
            (DiskModel model) -> findDiskDevice model
            (Partition _ _) -> throwString [i|block device cannot be a partition|]

--
-- TODO
--
copyDiskRootfsImage :: (MonadIO m, MonadReader env m, HasLogFunc env) => FilePath -> m ()
copyDiskRootfsImage = undefined
