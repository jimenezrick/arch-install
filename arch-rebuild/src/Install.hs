{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Install
    ( buildRootfs
    , copyDiskRootfsImage
    ) where

import RIO
import RIO.Directory
import RIO.FilePath (takeDirectory)

import qualified RIO.FilePath as F
import qualified RIO.Text as T

import Data.String.Interpolate
import Data.Text.IO (writeFile)

import Command
import Config
import Disk
import Filesystem
import Fstab

buildRootfs :: (MonadIO m, MonadReader env m, HasLogFunc env) => SystemConfig -> m ()
buildRootfs sysConf = do
    logInfo "Starting Arch Linux image build"
    createImgs
    formatImgs
    createDiskSubvols rootfsPath rootfsMnt $ fst <$> sysConf ^. storage . rootSubvolumes
    mountDiskSubvols rootfsPath rootfsMnt $ sysConf ^. storage . rootSubvolumes
    mountEsp
    bootstrapArch
    -- TODO: arch-chroot run settings
    -- TODO: prepare bootloader
    personalCustomization
    umountAllUnder rootfsMnt
  where
    espPath = sysConf ^. storage . espImage
    rootfsPath = sysConf ^. storage . rootfsImage
    rootfsMnt = takeDirectory rootfsPath </> "rootfs"
    espMnt = rootfsMnt </> "boot"
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
        let pkgList = T.unwords $ sysConf ^. packages . explicitPackages
            grpList = T.unwords $ sysConf ^. packages . packageGroups
        runCmd_ [i|pacstrap #{rootfsMnt} #{pkgList} #{grpList}|]
        let mirrorlistPath = rootfsMnt </> "/etc/pacman.d/mirrorlist"
        logInfo $ fromString [i|Copying mirrorlist to: #{mirrorlistPath}|]
        liftIO $ writeFile mirrorlistPath $ sysConf ^. packages . mirrorlist
        let fstabPath = rootfsMnt </> "/etc/fstab"
        logInfo $ fromString [i|Rendering fstab to: #{fstabPath}|]
        liftIO $ writeFile fstabPath =<< renderFstab (sysConf ^. storage . fstabEntries)
    personalCustomization = do
        logInfo $ fromString [i|Customizing rootfs on: #{rootfsMnt}|]
        createDirectoryIfMissing True $ rootfsMnt </> "/mnt/scratch"
        createDirectoryIfMissing True $ rootfsMnt </> "/mnt/garage"
        createDirectoryIfMissing True $ rootfsMnt </> "/mnt/usb"

createDiskSubvols ::
       (MonadIO m, MonadReader env m, HasLogFunc env) => FilePath -> FilePath -> [String] -> m ()
createDiskSubvols rootfsPath rootfsMnt subvols = do
    createDirectoryIfMissing False rootfsMnt
    logInfo $ fromString [i|Mounting rootfs on: #{rootfsMnt}|]
    mountLoopImage rootfsPath rootfsMnt
    logInfo $ fromString [i|Creating BTRFS subvolumes on #{rootfsPath}: [#{unwords subvols}]|]
    forM_ subvols $ \subvol -> runCmd_ [i|btrfs subvolume create #{rootfsMnt </> subvol}|]
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
        when (subvolPath /= "/") $ createDirectoryIfMissing True $ rootfsMnt </> subvolPath
        mountSubvol subvol rootfsPath (rootfsMnt </> subvolPath)

--
-- TODO: Review
--
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

(</>) :: FilePath -> FilePath -> FilePath
(</>) a b
    | F.isRelative b = a F.</> b
    | otherwise = a F.</> F.makeRelative "/" b
