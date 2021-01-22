{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Build where

import RIO hiding (lookup)
import RIO.Directory
import RIO.FilePath
import RIO.Process
import RIO.List (sortOn, isInfixOf)
import RIO.Map (fromList, lookup, member)

import qualified RIO.Text as T

import Data.String.Interpolate
import Data.Text.IO (writeFile)

import AUR
import Chroot
import Command
import Config
import Disk
import FilePath ((<//>))
import Filesystem
import FsTree
import Fstab

rootfsMnt :: FilePath
rootfsMnt = "/mnt/arch-rootfs"

wipeRootDisk :: (MonadUnliftIO m, MonadReader env m, HasProcessContext env, HasLogFunc env) => SystemConfig -> m ()
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

buildArch :: (MonadUnliftIO m, MonadReader env m, HasProcessContext env, HasLogFunc env) => LoadedSystemConfig -> Maybe FilePath -> Maybe FilePath -> m ()
buildArch loadedSysConf etcPath aurPkgsPath = do
    logInfo "Starting Arch Linux build"
    (espDev, rootfsDev) <- partitionDisk $ temporarySystemConfig loadedSysConf ^. storage . rootDisk
    let pass = temporarySystemConfig loadedSysConf ^. storage . passphrase
    withEncryptedRootfs rootfsDev pass $ \luksRootfsDev -> do
        makeFilesystemsPartitions espDev luksRootfsDev
        installInfo <- getRootDiskInstallInfo espDev rootfsDev
        let sysConf = resolveSystemConfig loadedSysConf installInfo
        buildRootfs sysConf espDev luksRootfsDev
            (\espMnt rootfsMnt -> do
                configureRootfsChroot sysConf rootfsMnt
                renderBootEntries sysConf espMnt
                case aurPkgsPath of
                    Nothing -> return ()
                    Just path -> do
                        pkgs <- filter (isInfixOf ".pkg.tar.") <$> listDirectory path
                        logInfo $ fromString [i|Installing pre-built AUR packages: #{pkgs}|]
                        installAURPackages rootfsMnt $ map (path </>) pkgs)
            (\_espMnt rootfsMnt ->
                case etcPath of
                    Nothing -> return ()
                    Just path -> do
                        logInfo $ fromString [i|Restoring /etc from: #{path}|]
                        runCmds_
                            [ [i|rm -r #{rootfsMnt </> "etc"}|]
                            , [i|git clone #{path} #{rootfsMnt </> "etc"}|]
                            , [i|chmod 700 #{rootfsMnt </> "etc/.git"}|]
                            , [i|arch-chroot #{rootfsMnt} bash -c '#{restoreEtcPermissions}'|]
                            ])
  where
    restoreEtcPermissions = "cd /etc; mtree -f .mtree -C -R time | mtree -U -X .mtree.exclude" :: String

buildRootfs ::
       (MonadIO m, MonadReader env m, HasProcessContext env, HasLogFunc env)
    => SystemConfig
    -> FilePath
    -> FilePath
    -> (FilePath -> FilePath -> m ())
    -> (FilePath -> FilePath -> m ())
    -> m ()
buildRootfs sysConf espDev rootfsDev preHook postHook = do
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
    -- Pre-hook
    preHook espMnt rootfsMnt
    -- Take snapshots
    takeSubvolSnapshot
        (fromList $ sysConf ^. storage . rootSubvolumes)
        ("@" :: FilePath)
        ("initial-build" :: FilePath)
    -- Post-hook
    postHook espMnt rootfsMnt
    renderFstab -- We generate again fstab on top of the restored /etc
    umountAllUnder rootfsMnt
  where
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
        renderFstab

    renderFstab = do
        let fstabPath = rootfsMnt <//> "/etc/fstab"
        logInfo $ fromString [i|Rendering fstab to: #{fstabPath}|]
        liftIO $ writeFile fstabPath =<< generateFstab (sysConf ^. storage . fstabEntries)

    renderMirrorlist servers = do
        logInfo $ fromString [i|Overwriting mirrorlist with: #{servers}|]
        liftIO $ writeFile "/etc/pacman.d/mirrorlist" . T.unlines $
            map (\s -> fromString [i|Server = #{s}|]) servers

    takeSubvolSnapshot subvols targetSubvol targetName =
        when (member "@snapshots" subvols) $ do
            logInfo $ fromString [i|Taking snapshot of BTRFS subvolume: #{targetSubvol}|]
            let Just snapshotsMnt = (rootfsMnt <//>) <$> lookup "@snapshots" subvols
                Just targetMnt = (rootfsMnt <//>) <$> lookup targetSubvol subvols
            runCmd_
                [i|btrfs subvolume snapshot -r #{targetMnt} #{snapshotsMnt <//> targetSubvol}-#{targetName}|]

partitionDisk ::
       (MonadIO m, MonadReader env m, HasProcessContext env, HasLogFunc env) => BlockDev -> m (FilePath, FilePath)
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
       (MonadIO m, MonadReader env m, HasProcessContext env, HasLogFunc env) => FilePath -> FilePath -> m ()
makeFilesystemsPartitions espDev rootfsDev = do
    logInfo $ fromString [i|Formatting ESP partition as FAT32: #{espDev}|]
    runCmd_ [i|mkfs.fat -F32 #{espDev}|]
    logInfo $ fromString [i|Formatting rootfs partition as BTRFS: #{rootfsDev}|]
    runCmd_ [i|mkfs.btrfs #{rootfsDev}|]

withEncryptedRootfs ::
       (MonadUnliftIO m, MonadReader env m, HasProcessContext env, HasLogFunc env)
    => FilePath
    -> Text
    -> (FilePath -> m ())
    -> m ()
withEncryptedRootfs rootfsDev passphrase f = do
    let luksDevName = "cryptroot" :: String
    bracket
        (do logInfo $ fromString [i|Encrypting rootfs partition with LUKS: #{rootfsDev}|]
            runCmds_
                [ [i|cryptsetup -q -v --type luks2 --key-file <(echo "#{passphrase}") luksFormat #{rootfsDev}|]
                , [i|cryptsetup --persistent --allow-discards --key-file <(echo "#{passphrase}") open #{rootfsDev} #{luksDevName}|]
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
