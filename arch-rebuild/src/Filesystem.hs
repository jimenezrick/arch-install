{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}

module Filesystem where

import RIO

import Data.String.Interpolate

import Command

createZeroImage :: MonadIO m => FilePath -> Int -> m ()
createZeroImage path sizeMegas = runCmd_ [i|dd if=/dev/zero of=#{path} bs=1M count=#{sizeMegas}|]

mountLoopImage :: MonadIO m => FilePath -> FilePath -> m ()
mountLoopImage imgPath mntPoint = runCmd_ [i|mount -o loop #{imgPath} #{mntPoint}|]

umountPoint :: MonadIO m => FilePath -> m ()
umountPoint mntPoint = runCmd_ [i|umount #{mntPoint}|]
