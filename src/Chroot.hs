{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Chroot where

import RIO
import RIO.Directory
import RIO.FilePath (makeRelative, replaceDirectory, replaceFileName, takeFileName)

import Data.String.Interpolate
import UnliftIO.Environment (getExecutablePath)

import Command
import Config
import FilePath ((<//>))

runBinInChroot ::
       (MonadIO m, MonadReader env m, HasLogFunc env) => SystemConfig -> FilePath -> String -> m ()
runBinInChroot sysConf rootfsMnt binCmd = do
    (chrootBin, chrootConf) <- copyExecutableWithBuildInfo sysConf rootfsMnt
    let chrootBin' = makeRelative rootfsMnt chrootBin
        chrootConf' = makeRelative rootfsMnt chrootConf
    logInfo $ fromString [i|Running within chroot #{rootfsMnt}: #{binCmd}|]
    runCmd_ [i|arch-chroot #{rootfsMnt} #{chrootBin'} #{binCmd} --bin-conf-path #{chrootConf'}|]

configureRootfsChroot ::
       (MonadIO m, MonadReader env m, HasLogFunc env) => SystemConfig -> FilePath -> m ()
configureRootfsChroot sysConf rootfsMnt = runBinInChroot sysConf rootfsMnt "configure-rootfs"

configureRootfs :: (MonadIO m, MonadReader env m, HasLogFunc env) => SystemConfig -> m ()
configureRootfs SystemConfig {..} =
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
        , [i|bootctl install|]
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

copyExecutableWithBuildInfo ::
       (MonadIO m, MonadReader env m, HasLogFunc env)
    => SystemConfig
    -> FilePath
    -> m (FilePath, FilePath)
copyExecutableWithBuildInfo sysConf rootfsMnt = do
    execPath <- getExecutablePath
    let execName = takeFileName execPath
        chrootDest = rootfsMnt <//> execName
        execDest = replaceDirectory execPath chrootDest
        confDest = replaceFileName execDest "system-build.info"
    isCopied <- doesDirectoryExist chrootDest
    unless isCopied $ do
        logInfo $ fromString [i|Copying binary to chroot: #{rootfsMnt}|]
        createDirectory chrootDest
        copyFile execPath execDest
        saveBinSystemConfig confDest sysConf
    return (execDest, confDest)
