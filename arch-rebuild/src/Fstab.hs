{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Fstab where

import RIO hiding ((^.), to, unlines)
import RIO.Map (fromList)
import RIO.Text (pack, unlines)

import Control.Error ((??))
import Control.Lens hiding ((??))
import Control.Monad.Except
import Data.String.Interpolate

import Config
import Disk

renderFstab :: MonadIO m => [FstabEntry] -> ExceptT String m Text
renderFstab entries = unlines . map pack . concat <$> mapM renderDev entries
  where
    renderDev entry =
        case entry ^. fsEntry of
            DiskModel {_model} -> do
                disk <- getDiskInfo _model
                case disk of
                    DiskWithPartitionsInfo {} -> throwError "expecting a disk without partitions"
                    DiskInfo {uuid} ->
                        return
                            [ [i|# #{_model}|]
                            , [i|UUID=#{uuid} #{entry^.fsMountPoint} #{entry^.fsType} #{entry^.fsOpts} #{entry^.fsDump} #{entry^.fsck}|]
                            ]
            Partition {_diskModel, _partNum} -> do
                disk <- getDiskInfo _diskModel
                case disk of
                    DiskInfo {} -> throwError "expecting a disk with partitions"
                    DiskWithPartitionsInfo {partitions} -> do
                        dev <- findDiskDevice _diskModel
                        uuid <-
                            (fromList [(d, u) | (PartitionInfo d u _) <- partitions] ^.
                             at [i|#{dev}#{_partNum}|]) ??
                            "missing expected partition"
                        return
                            [ [i|# #{_diskModel}|]
                            , [i|UUID=#{uuid} #{entry^.fsMountPoint} #{entry^.fsType} #{entry^.fsOpts} #{entry^.fsDump} #{entry^.fsck}|]
                            ]
            DevPath {_path} ->
                return
                    [ [i|#{_path} #{entry^.fsMountPoint} #{entry^.fsType} #{entry^.fsOpts} #{entry^.fsDump} #{entry^.fsck}|]
                    ]
