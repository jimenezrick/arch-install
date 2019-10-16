{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Chroot where

import RIO
import RIO.Directory
import RIO.FilePath ((<.>), makeRelative, replaceDirectory)

import Data.String.Interpolate
import UnliftIO.Environment (getExecutablePath)

import Command
import Config
import Util

runBinInChroot ::
       (MonadIO m, MonadReader env m, HasLogFunc env) => SystemConfig -> FilePath -> String -> m ()
runBinInChroot sysConf rootfsMnt binCmd = do
    logInfo $ fromString [i|Copying binary to chroot: #{rootfsMnt}|]
    let chrootDest = rootfsMnt </> "arch-rebuild"
    createDirectory chrootDest
    (chrootBin, chrootConf) <- copyExecutableWithConfig sysConf chrootDest
    let chrootBin' = makeRelative rootfsMnt chrootBin
        chrootConf' = makeRelative rootfsMnt chrootConf
    logInfo $ fromString [i|Running inside chroot #{rootfsMnt}: #{binCmd}|]
    runCmd_ [i|arch-chroot #{rootfsMnt} #{chrootBin'} #{binCmd} --bin-conf-path #{chrootConf'}|]
    logInfo $ fromString [i|Cleaning up binary in chroot: #{rootfsMnt}|]
    removeDirectoryRecursive chrootDest

installBootloaderChroot ::
       (MonadIO m, MonadReader env m, HasLogFunc env) => SystemConfig -> FilePath -> m ()
installBootloaderChroot sysConf rootfsMnt =
    runBinInChroot sysConf rootfsMnt "install-bootloader-chroot"

installBootloader :: (MonadIO m, MonadReader env m, HasLogFunc env) => SystemConfig -> m ()
installBootloader SystemConfig {..} = runCmd_ [i|bootctl install|]

configureArchChroot ::
       (MonadIO m, MonadReader env m, HasLogFunc env) => SystemConfig -> FilePath -> m ()
configureArchChroot sysConf rootfsMnt = runBinInChroot sysConf rootfsMnt "configure-chroot"

configureArch :: (MonadIO m, MonadReader env m, HasLogFunc env) => SystemConfig -> m ()
configureArch SystemConfig {..} =
    runCmds_
        [ [i|ln -sf /usr/share/zoneinfo/#{_zoneInfo} /etc/localtime|]
        , [i|hwclock --systohc|]
        , [i|sed -i "s/##{_locale}.UTF-8 UTF-8/#{_locale}.UTF-8 UTF-8/" /etc/locale.gen|]
        , [i|locale-gen|]
        , [i|echo LANG=#{_locale}.UTF-8 >/etc/locale.conf|]
        , [i|echo KEYMAP=#{_keymap} >/etc/vconsole.conf|]
        , [i|echo #{_hostname} >/etc/hostname|]
        , [i|sed -i "s/^HOOKS=.*/HOOKS=(#{unwords hooks})/" /etc/mkinitcpio.conf|]
        , [i|mkinitcpio -p linux|]
        , [i|mkinitcpio -p linux-lts|]
        ]
  where
    hooks =
        [ "base"
        , "systemd"
        , "autodetect"
        , "keyboard"
        , "sd-vconsole"
        , "modconf"
        , "block"
        , "sd-encrypt"
        , "filesystems"
        , "fsck"
        , "sd-shutdown"
        ]
        -- FIXME: shutdown error (https://github.com/systemd/systemd/issues/8155)

copyExecutableWithConfig :: MonadIO m => SystemConfig -> FilePath -> m (FilePath, FilePath)
copyExecutableWithConfig sysConf destPath = do
    execPath <- getExecutablePath
    let execDest = replaceDirectory execPath destPath
        confDest = execDest <.> "conf"
    copyFile execPath execDest
    saveBinSystemConfig confDest sysConf
    return (execDest, confDest)
