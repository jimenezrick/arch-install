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

import Config hiding (uuid)
import Disk
import Error

renderFstab :: MonadIO m => [FstabEntry] -> m Text
renderFstab entries = unlines . map pack . concat <$> mapM renderDev entries
  where
    renderDev entry =
        case entry ^. fsEntry of
            DiskModel {_model} -> do
                disk <- getDiskInfo _model
                case disk of
                    DiskWithPartitionsInfo {} -> throwString "expecting a disk without partitions"
                    DiskInfo {uuid} ->
                        return
                            [ [i|# #{_model}|]
                            , [i|UUID=#{uuid} #{entry^.fsMountPoint} #{entry^.fsType} #{entry^.fsOpts} #{entry^.fsDump} #{entry^.fsck}|]
                            ]
            Partition {_diskModel, _partNum} -> do
                disk <- getDiskInfo _diskModel
                case disk of
                    DiskInfo {} -> throwString "expecting a disk with partitions"
                    DiskWithPartitionsInfo {partitions} -> do
                        dev <- findDiskDevice _diskModel
                        uuid <-
                            throwNothing "missing expected partition" $
                            return
                                (fromList [(d, u) | (PartitionInfo d u _) <- partitions] ^.
                                 at [i|#{dev}#{_partNum}|])
                        return
                            [ [i|# #{_diskModel}|]
                            , [i|UUID=#{uuid} #{entry^.fsMountPoint} #{entry^.fsType} #{entry^.fsOpts} #{entry^.fsDump} #{entry^.fsck}|]
                            ]
            DevPath {_path} ->
                return
                    [ [i|#{_path} #{entry^.fsMountPoint} #{entry^.fsType} #{entry^.fsOpts} #{entry^.fsDump} #{entry^.fsck}|]
                    ]
