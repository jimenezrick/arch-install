{-# LANGUAGE QuasiQuotes #-}

module Filesystem where

import RIO

import Data.String.Interpolate

import Command

createZeroImage :: MonadIO m => FilePath -> Text -> m ()
createZeroImage path size = runCmd_ [i|truncate -s #{size} #{path}|]

mountLoopImage :: MonadIO m => FilePath -> FilePath -> m ()
mountLoopImage imgPath mntPoint = runCmd_ [i|mount -o loop #{imgPath} #{mntPoint}|]

mountSubvol :: MonadIO m => FilePath -> FilePath -> String -> m ()
mountSubvol subvol devPath mntPoint = runCmd_ [i|mount -o subvol=#{subvol} #{devPath} #{mntPoint}|]

mountPoint :: MonadIO m => FilePath -> FilePath -> m ()
mountPoint devPath mntPoint = runCmd_ [i|mount #{devPath} #{mntPoint}|]

umountPoint :: MonadIO m => FilePath -> m ()
umountPoint mntPoint = runCmd_ [i|umount #{mntPoint}|]

umountAllUnder :: MonadIO m => FilePath -> m ()
umountAllUnder mntPoint = runCmd_ [i|umount --recursive #{mntPoint}|]
