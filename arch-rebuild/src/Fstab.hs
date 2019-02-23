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

-- TODO: space between generated lines
renderFstab :: MonadIO m => [FstabEntry] -> ExceptT String m Text
renderFstab entries = unlines . map pack . concat <$> mapM renderDev entries
  where
    renderDev entry =
        case entry ^. fsEntry of
            Disk {_model} -> do
                disk <- getDiskInfo _model
                case disk of
                    DiskWithPartitionsInfo {} -> throwError "expecting a disk without partitions"
                    DiskInfo {uuid} ->
                        return
                            [ [i|# #{_model}|]
                            , [i|UUID=#{uuid} #{entry^.fsMountPoint} #{entry^.fsType} #{entry^.fsOpts} #{entry^.fsDump} #{entry^.fsck}|]
                            ]
            Partition {_model, _number} -> do
                disk <- getDiskInfo _model
                case disk of
                    DiskInfo {} -> throwError "expecting a disk with partitions"
                    DiskWithPartitionsInfo {partitions} -> do
                        dev <- findDiskDevice _model
                        uuid <-
                            (fromList [(d, u) | (PartitionInfo d u _) <- partitions] ^.
                             at [i|#{dev}#{_number}|]) ??
                            "missing expected partition"
                        return
                            [ [i|# #{_model}|]
                            , [i|UUID=#{uuid} #{entry^.fsMountPoint} #{entry^.fsType} #{entry^.fsOpts} #{entry^.fsDump} #{entry^.fsck}|]
                            ]
            Device {_path} ->
                return
                    [ [i|#{_path} #{entry^.fsMountPoint} #{entry^.fsType} #{entry^.fsOpts} #{entry^.fsDump} #{entry^.fsck}|]
                    ]
