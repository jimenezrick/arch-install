{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Install where

import RIO
import RIO.Directory
import RIO.FilePath (takeDirectory)

import qualified RIO.Text as T

import Data.String.Interpolate
import Data.Text.IO (writeFile)

import Chroot
import Command
import Config
import Disk
import FilePath ((<//>))
import Filesystem
import Fstab

buildArch :: (MonadIO m, MonadReader env m, HasLogFunc env) => SystemConfig -> m ()
buildArch sysConf = do
    partitionDisk $ sysConf ^. storage . rootDisk -- TODO: LUKS

buildRootfs :: (MonadIO m, MonadReader env m, HasLogFunc env) => SystemConfig -> m ()
buildRootfs sysConf = do
    logInfo "Starting Arch Linux image build"
    createImgs
    formatImgs
    createDiskSubvols rootfsPath rootfsMnt $ fst <$> sysConf ^. storage . rootSubvolumes
    mountDiskSubvols rootfsPath rootfsMnt $ sysConf ^. storage . rootSubvolumes
    mountEsp
    bootstrapArch
    configureRootfsChroot sysConf rootfsMnt
    umountAllUnder rootfsMnt
  where
    espPath = sysConf ^. storage . espImage
    rootfsPath = sysConf ^. storage . rootfsImage
    rootfsMnt = takeDirectory rootfsPath <//> "rootfs"
    espMnt = rootfsMnt <//> "boot"
    createImgs = do
        logInfo $ fromString [i|Creating ESP image: #{espPath}|]
        createZeroImage espPath $ sysConf ^. storage . espImageSize
        logInfo $ fromString [i|Creating rootfs image: #{rootfsPath}|]
        createZeroImage rootfsPath $ sysConf ^. storage . rootfsImageSize
    formatImgs = do
        logInfo $ fromString [i|Formatting ESP: #{espPath}|]
        runCmd_ [i|mkfs.fat -F32 #{espPath}|]
        logInfo $ fromString [i|Formatting rootfs partition: #{rootfsPath}|]
        runCmd_ [i|mkfs.btrfs -f #{rootfsPath}|]
    mountEsp = do
        createDirectoryIfMissing False espMnt
        logInfo $ fromString [i|Mounting ESP on: #{espMnt}|]
        mountLoopImage espPath espMnt
    bootstrapArch = do
        logInfo $ fromString [i|Bootstrapping Arch on: #{rootfsMnt}|]
        let pkgList = T.unwords $ sysConf ^. pacman . packages
            grpList = T.unwords $ sysConf ^. pacman . groups
        runCmd_ [i|pacstrap #{rootfsMnt} #{pkgList} #{grpList}|]
        let mirrorlistPath = rootfsMnt <//> "/etc/pacman.d/mirrorlist"
        logInfo $ fromString [i|Copying mirrorlist to: #{mirrorlistPath}|]
        liftIO $ writeFile mirrorlistPath $ sysConf ^. pacman . mirrorlist
        let fstabPath = rootfsMnt <//> "/etc/fstab"
        logInfo $ fromString [i|Rendering fstab to: #{fstabPath}|]
        liftIO $ writeFile fstabPath =<< renderFstab (sysConf ^. storage . fstabEntries)

createDiskSubvols ::
       (MonadIO m, MonadReader env m, HasLogFunc env) => FilePath -> FilePath -> [String] -> m ()
createDiskSubvols rootfsPath rootfsMnt subvols = do
    createDirectoryIfMissing False rootfsMnt
    logInfo $ fromString [i|Mounting rootfs on: #{rootfsMnt}|]
    mountLoopImage rootfsPath rootfsMnt
    logInfo $ fromString [i|Creating BTRFS subvolumes on #{rootfsPath}: [#{unwords subvols}]|]
    forM_ subvols $ \subvol -> runCmd_ [i|btrfs subvolume create #{rootfsMnt <//> subvol}|]
    umountPoint rootfsMnt

mountDiskSubvols ::
       (MonadIO m, MonadReader env m, HasLogFunc env)
    => FilePath
    -> FilePath
    -> [(String, FilePath)]
    -> m ()
mountDiskSubvols rootfsPath rootfsMnt subvols = do
    logInfo $
        fromString
            [i|Mounting BTRFS subvolumes of #{rootfsPath}: [#{unwords $ map (\(v,p) -> v ++ ":" ++ p) subvols}]|]
    forM_ subvols $ \(subvol, subvolPath) -> do
        when (subvolPath /= "/") $ createDirectoryIfMissing True $ rootfsMnt <//> subvolPath
        mountSubvol subvol rootfsPath (rootfsMnt <//> subvolPath)

--
-- TODO: Review
--
partitionDisk :: (MonadIO m, MonadReader env m, HasLogFunc env) => BlockDev -> m ()
partitionDisk other
    | (FsUUID _) <- other = invalid
    | (PartUUID _) <- other = invalid
    | (DiskModelPartition _ _) <- other = invalid
  where
    invalid = throwString "Block device needs to be a disk"
partitionDisk blockdev = do
    dev <- findDiskDevice blockdev
    let espDev = [i|#{dev}1|]
        rootfsDev = [i|#{dev}2|]
    logInfo $ fromString [i|Partitioning device: #{dev}|]
    runCmd_ $ [i|parted -s #{dev} -a optimal|] ++
        " mklabel gpt \
        \ mkpart primary 0% 513MiB \
        \ mkpart primary 513MiB 100% \
        \ set 1 boot on"
    -- TODO: do this in a different function, received as argument the device
    logInfo $ fromString [i|Formatting ESP partition as FAT32: #{espDev}|]
    runCmd_ [i|mkfs.fat -F32 #{espDev}|]
    logInfo $ fromString [i|Formatting rootfs partition as BTRFS: #{rootfsDev}|]
    runCmd_ [i|mkfs.btrfs #{rootfsDev}|]
    --
    -- TODO: btrfs subvols, in a different function
    --
