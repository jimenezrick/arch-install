{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Install where

import RIO
import RIO.Directory
import RIO.FilePath
import RIO.List

import qualified RIO.Text as T

import Data.String.Interpolate
import Data.Text.IO (appendFile, writeFile)

import Chroot
import Command
import Config
import Disk
import FilePath ((<//>))
import Filesystem
import FsTree
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

buildArch :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env) => LoadedSystemConfig -> m ()
buildArch loadedSysConf = do
    logInfo "Starting Arch Linux build"
    (espDev, rootfsDev) <- partitionDisk $ temporarySystemConfig loadedSysConf ^. storage . rootDisk
    withEncryptedRootfs rootfsDev $ \luksRootfsDev -> do
        makeFilesystemsPartitions espDev luksRootfsDev
        installInfo <- getRootDiskInstallInfo espDev rootfsDev
        let sysConf = resolveSystemConfig loadedSysConf installInfo
        buildRootfs sysConf espDev luksRootfsDev $ \espMnt rootfsMnt -> do
            configureRootfsChroot sysConf rootfsMnt
            renderBootEntries sysConf espMnt
            -- TODO: take BTRFS snapshots after installation here and mount it somewhere?

buildRootfs ::
       (MonadIO m, MonadReader env m, HasLogFunc env)
    => SystemConfig
    -> FilePath
    -> FilePath
    -> (FilePath -> FilePath -> m ())
    -> m ()
buildRootfs sysConf espDev rootfsDev f = do
    createDirectoryIfMissing False rootfsMnt
    -- Mount rootfs and create subvols
    mountPoint rootfsDev rootfsMnt
    createDiskSubvols $ fst <$> sysConf ^. storage . rootSubvolumes
    umountPoint rootfsMnt
    -- Mount rootfs subvols and ESP
    mountDiskSubvols . sortOn fst $ sysConf ^. storage . rootSubvolumes
    createDirectoryIfMissing False espMnt
    mountPoint espDev espMnt
    -- Bootstrap Arch
    maybe (return ()) renderMirrorlist $ sysConf ^. pacman . mirrorlist
    bootstrapArch
    f espMnt rootfsMnt
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
        let fstabPath = rootfsMnt <//> "/etc/fstab"
        logInfo $ fromString [i|Rendering fstab to: #{fstabPath}|]
        liftIO $ appendFile fstabPath =<< renderFstab (sysConf ^. storage . fstabEntries)
    renderMirrorlist servers = do
        logInfo $ fromString [i|Overwriting mirrorlist with: #{servers}|]
        liftIO $ writeFile "/etc/pacman.d/mirrorlist" . T.unlines $
            map (\s -> fromString [i|Server = #{s}|]) servers

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
    -> m ()
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
        f

renderBootEntries ::
       (MonadIO m, MonadReader env m, HasLogFunc env) => SystemConfig -> FilePath -> m ()
renderBootEntries sysConf espMnt = do
    let entries = sysConf ^. storage . boot . bootEntries
        loader = sysConf ^. storage . boot . loaderConf
        entryFiles =
            map (\(name, entry) -> File (T.unpack name <.> "conf") (Content entry) defAttrs) entries
    logInfo $ fromString [i|Rendering EFI boot entries: #{map fst entries}|]
    createFsTreeAt espMnt $
        Dir
            "loader"
            defAttrs
            [File "loader.conf" (Content loader) defAttrs, Dir "entries" defAttrs entryFiles]
