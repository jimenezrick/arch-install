{-# LANGUAGE QuasiQuotes #-}

module Filesystem where

import RIO
import RIO.Process

import Data.String.Interpolate

import Command

mountSubvol :: (MonadIO m, MonadReader env m, HasProcessContext env) => FilePath -> FilePath -> String -> m ()
mountSubvol subvol devPath mntPoint = runCmd_ [i|mount -o subvol=#{subvol} #{devPath} #{mntPoint}|]

mountPoint :: (MonadIO m, MonadReader env m, HasProcessContext env) => FilePath -> FilePath -> m ()
mountPoint devPath mntPoint = runCmd_ [i|mount #{devPath} #{mntPoint}|]

umountPoint :: (MonadIO m, MonadReader env m, HasProcessContext env) => FilePath -> m ()
umountPoint mntPoint = runCmd_ [i|umount #{mntPoint}|]

umountAllUnder :: (MonadIO m, MonadReader env m, HasProcessContext env) => FilePath -> m ()
umountAllUnder mntPoint = runCmd_ [i|umount --recursive #{mntPoint}|]
