{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Install where

import RIO hiding (threadDelay, unwords)
import RIO.Directory
import RIO.FilePath ((</>))

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
    -- TODO: create btrfs subvols
    bootstrapArch
    -- TODO: arch-chroot run settings
    -- TODO: prepare bootloader
    umountImgs
  where
    rootfsMnt = "/mnt/rootfs"
    espMnt = rootfsMnt </> "boot"
    espPath = installConf ^. espImage
    rootfsPath = installConf ^. rootfsImage
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
        createDirectoryIfMissing False espMnt
        logInfo $ fromString [i|Mounting rootfs on: #{rootfsMnt}|]
        mountLoopImage rootfsPath rootfsMnt
        logInfo $ fromString [i|Mounting ESP on: #{espMnt}|]
        mountLoopImage espPath espMnt
    bootstrapArch = do
        logInfo $ fromString [i|Bootstrapping Arch on: #{rootfsMnt}|]
        let packages = unwords $ installConf ^. system . pacman . explicitPackages
            groups = unwords $ installConf ^. system . pacman . packageGroups
        runCmd_ [i|pacstrap #{rootfsMnt} #{packages} #{groups}|]
        logInfo $ fromString [i|Copying mirrorlist|]
        liftIO $ writeFile (rootfsMnt </> "/etc/pacman.d/mirrorlist") $ installConf ^. system .
            pacman .
            mirrorlist
        logInfo $ fromString [i|Rendering fstab|]
        liftIO $ writeFile (rootfsMnt </> "/etc/fstab") =<<
            renderFstab (installConf ^. system . fstabEntries)
    umountImgs = do
        umountPoint espMnt
        umountPoint rootfsMnt

--
-- TODO: Finish and take a BlockDev as argument
--
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

--
-- TODO
--
copyDiskRootfsImage :: (MonadIO m, MonadReader env m, HasLogFunc env) => FilePath -> m ()
copyDiskRootfsImage = undefined
