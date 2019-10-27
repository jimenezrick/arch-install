{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Install where

import RIO
import RIO.Directory
import RIO.List

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

wipeRootDisk :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env) => SystemConfig -> m ()
wipeRootDisk sysConf = do
    rootDiskInfo <- getDiskInfo $ sysConf ^. storage . rootDisk
    case rootDiskInfo of
        DiskInfo {dev} -> wipe dev
        DiskWithPartitionsInfo {dev, partitions} -> do
            mapM_ (wipe . partDev) partitions
            wipe dev
  where
    wipe dev = do
        logInfo $ fromString [i|Wiping device: #{dev}|]
        runCmd_ [i|wipefs -a #{dev}|]

buildArch :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env) => SystemConfig -> m ()
buildArch sysConf = do
    logInfo "Starting Arch Linux build"
    (espDev, rootfsDev) <- partitionDisk $ sysConf ^. storage . rootDisk
    luksRootfsDev <-
        withEncryptedRootfs rootfsDev $ \luksRootfsDev -> do
            makeFilesystemsPartitions espDev luksRootfsDev
            buildRootfs sysConf espDev luksRootfsDev
    return ()

buildRootfs ::
       (MonadIO m, MonadReader env m, HasLogFunc env)
    => SystemConfig
    -> FilePath
    -> FilePath
    -> m ()
buildRootfs sysConf espDev rootfsDev = do
    createDirectoryIfMissing False rootfsMnt
    mountPoint rootfsDev rootfsMnt
    createDiskSubvols $ fst <$> sysConf ^. storage . rootSubvolumes
    umountPoint rootfsMnt
    --
    mountDiskSubvols . sortOn fst $ sysConf ^. storage . rootSubvolumes
    createDirectoryIfMissing False espMnt
    mountPoint espDev espMnt
    --
    bootstrapArch
    configureRootfsChroot sysConf rootfsMnt
    --
    umountAllUnder rootfsMnt
  where
    rootfsMnt = "/mnt/arch-rootfs"
    espMnt = rootfsMnt <//> "boot"
    createDiskSubvols subvols = do
        logInfo $ fromString [i|Creating BTRFS subvolumes on: #{rootfsDev}|]
        forM_ subvols $ \subvol -> runCmd_ [i|btrfs subvolume create #{rootfsMnt <//> subvol}|]
    mountDiskSubvols subvols = do
        logInfo $
            fromString
                [i|Mounting BTRFS subvolumes of #{rootfsDev}: [#{unwords $ map (\(v,p) -> v ++ ":" ++ p) subvols}]|]
        forM_ subvols $ \(subvol, subvolPath) -> do
            when (subvolPath /= "/") $ createDirectoryIfMissing True $ rootfsMnt <//> subvolPath
            mountSubvol subvol rootfsDev (rootfsMnt <//> subvolPath)
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

partitionDisk ::
       (MonadIO m, MonadReader env m, HasLogFunc env) => BlockDev -> m (FilePath, FilePath)
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
    runCmd_ $ [i|parted -s #{dev} -a optimal |] ++
        unwords
            [ "mklabel gpt"
            , "mkpart primary 0% 513MiB"
            , "mkpart primary 513MiB 100%"
            , "set 1 boot on"
            ]
    return (espDev, rootfsDev)

makeFilesystemsPartitions ::
       (MonadIO m, MonadReader env m, HasLogFunc env) => FilePath -> FilePath -> m ()
makeFilesystemsPartitions espDev rootfsDev = do
    logInfo $ fromString [i|Formatting ESP partition as FAT32: #{espDev}|]
    runCmd_ [i|mkfs.fat -F32 #{espDev}|]
    logInfo $ fromString [i|Formatting rootfs partition as BTRFS: #{rootfsDev}|]
    runCmd_ [i|mkfs.btrfs #{rootfsDev}|]

withEncryptedRootfs ::
       (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
    => FilePath
    -> (FilePath -> m ())
    -> m FilePath
withEncryptedRootfs rootfsDev f = do
    let luksDevName = "cryptroot" :: String
    bracket
        (do logInfo $ fromString [i|Encrypting rootfs partition with LUKS: #{rootfsDev}|]
            runCmds_
                [ [i|cryptsetup -y -v luksFormat --type luks2 #{rootfsDev}|]
                , [i|cryptsetup --persistent --allow-discards open #{rootfsDev} #{luksDevName}|]
                ]
            return [i|/dev/mapper/#{luksDevName}|])
        (\_ -> runCmd_ [i|cryptsetup close #{luksDevName}|])
        (\luksDev -> f luksDev >> return luksDev)
