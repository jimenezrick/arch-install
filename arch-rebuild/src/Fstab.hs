{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Fstab where

import RIO hiding ((^.), to, unlines)
import RIO.Map (fromList)
import RIO.Text (pack, unlines)

import Control.Lens hiding ((??))
import Data.String.Interpolate

import Config
import Error

import qualified Disk

renderFstab :: MonadUnliftIO m => [FstabEntry] -> m Text
renderFstab entries = unlines . map pack . concat <$> mapM renderDev entries
  where
    formatEntry fsSpec entry =
        [i|#{fsSpec} #{entry^.fsMountPoint} #{entry^.fsType} #{entry^.fsOpts} #{entry^.fsDump} #{entry^.fsck}|]
    renderDev entry =
        case entry ^. fsEntry of
            FsUUID {_uuid} -> return [formatEntry [i|UUID=#{_uuid}|] entry]
            DevPath {_path} -> return [formatEntry [i|#{_path}|] entry]
            blockdev@(DiskModel _model) -> do
                disk <- Disk.getDiskInfo blockdev
                case disk of
                    Disk.DiskWithPartitionsInfo {} ->
                        throwString "Expecting a disk without partitions"
                    Disk.DiskInfo {Disk.uuid} ->
                        return [[i|# #{_model}|], formatEntry [i|UUID=#{uuid}|] entry]
            blockdev@(DiskModelPartition {..}) -> do
                disk <- Disk.getDiskInfo blockdev
                case disk of
                    Disk.DiskInfo {} -> throwString "Expecting a disk with partitions"
                    Disk.DiskWithPartitionsInfo {Disk.partitions} -> do
                        dev <- Disk.findDiskModelDevice _diskModel
                        uuid <-
                            throwNothing "missing expected partition" $
                            return
                                (fromList [(d, u) | (Disk.PartitionInfo d u _ _) <- partitions] ^.
                                 at [i|#{dev}#{_partNum}|])
                        return [[i|# #{_diskModel}|], formatEntry [i|UUID=#{uuid}|] entry]
