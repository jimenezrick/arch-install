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

generateFstab :: MonadUnliftIO m => [FstabEntry] -> m Text
generateFstab entries = unlines . map pack . concat <$> mapM renderDev entries
  where
    formatEntry fsSpec entry =
        [i|#{fsSpec} #{entry^.fsMountPoint} #{entry^.fsType} #{entry^.fsOpts} #{entry^.fsDump} #{entry^.fsck}|]
    renderDev entry =
        case entry ^. fsEntry of
            FsUUID {_uuid} -> return [formatEntry [i|UUID=#{_uuid}|] entry]
            PartUUID {_partUuid} -> return [formatEntry [i|PARTUUID=#{_partUuid}|] entry]
            DevPath {_path} -> return [formatEntry [i|#{_path}|] entry]
            blockdev@(DiskModel _model) -> do
                disk <- Disk.getDiskInfo blockdev
                case disk of
                    Disk.DiskWithPartitionsInfo {} ->
                        throwString "Expecting a disk without partitions"
                    Disk.DiskInfo {Disk.uuid} ->
                        return [[i|# #{_model}|], formatEntry [i|UUID=#{uuid}|] entry]
            blockdev@DiskModelPartition {..} -> do
                disk <- Disk.getDiskInfo blockdev
                case disk of
                    Disk.DiskInfo {} -> throwString "Expecting a disk with partitions"
                    Disk.DiskWithPartitionsInfo {Disk.partitions} -> do
                        dev <- Disk.findDiskModelDevice _diskModel
                        uuid <-
                            throwNothing "missing expected partition" $
                            return
                                (fromList
                                     [ (partDev, uuid)
                                     | Disk.PartitionInfo {partDev, uuid} <- partitions
                                     ] ^.
                                 at [i|#{dev}#{_partNum}|])
                        return [[i|# #{_diskModel}|], formatEntry [i|UUID=#{uuid}|] entry]
