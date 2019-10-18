{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Fstab where

import RIO hiding ((^.), to, unlines)
import RIO.Map (fromList)
import RIO.Text (pack, unlines)

import Control.Lens hiding ((??))
import Data.String.Interpolate

import Config
import Error

import qualified Disk

renderFstab :: MonadIO m => [FstabEntry] -> m Text
renderFstab entries = unlines . map pack . concat <$> mapM renderDev entries
  where
    formatEntry fsSpec entry =
        [i|#{fsSpec} #{entry^.fsMountPoint} #{entry^.fsType} #{entry^.fsOpts} #{entry^.fsDump} #{entry^.fsck}|]
    renderDev entry =
        case entry ^. fsEntry of
            FsUUID {_uuid} -> return [formatEntry [i|UUID=#{_uuid}|] entry]
            DevPath {_path} -> return [formatEntry [i|#{_path}|] entry]
            DiskModel {_model} -> do
                disk <- Disk.findDiskDevice _model >>= Disk.getDiskInfo
                case disk of
                    Disk.DiskWithPartitionsInfo {} ->
                        throwString "expecting a disk without partitions"
                    Disk.DiskInfo {Disk.uuid} ->
                        return [[i|# #{_model}|], formatEntry [i|UUID=#{uuid}|] entry]
            Partition {_diskModel, _partNum} -> do
                disk <- Disk.findDiskDevice _diskModel >>= Disk.getDiskInfo
                case disk of
                    Disk.DiskInfo {} -> throwString "expecting a disk with partitions"
                    Disk.DiskWithPartitionsInfo {Disk.partitions} -> do
                        dev <- Disk.findDiskDevice _diskModel
                        uuid <-
                            throwNothing "missing expected partition" $
                            return
                                (fromList [(d, u) | (Disk.PartitionInfo d u _) <- partitions] ^.
                                 at [i|#{dev}#{_partNum}|])
                        return [[i|# #{_diskModel}|], formatEntry [i|UUID=#{uuid}|] entry]
