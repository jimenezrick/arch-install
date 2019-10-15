{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Chroot where

import RIO

import Data.String.Interpolate

import Command
import Config

prepareChroot :: (MonadIO m, MonadReader env m, HasLogFunc env) => SystemConfig -> m ()
prepareChroot SystemConfig {..} =
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
