{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}

module Filesystem where

import RIO

import Data.String.Interpolate

import Command

createZeroImage :: MonadIO m => FilePath -> Int -> m ()
createZeroImage path sizeGigas = runCmd_ [i|truncate -s #{sizeGigas}G #{path}|]

mountLoopImage :: MonadIO m => FilePath -> FilePath -> m ()
mountLoopImage imgPath mntPoint = runCmd_ [i|mount -o loop #{imgPath} #{mntPoint}|]

umountPoint :: MonadIO m => FilePath -> m ()
umountPoint mntPoint = runCmd_ [i|umount #{mntPoint}|]
