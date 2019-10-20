{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Disk where

import RIO hiding (to, words)
import RIO.FilePath
import RIO.Process

import Control.Lens hiding (children)
import Data.Aeson (Value(..), eitherDecode')
import Data.Aeson.Lens
import Data.String.Interpolate
import Data.Text.Lens (_Text)
import Data.UUID (UUID, fromText)

import Error

data InstallDiskInfo = InstallDiskInfo
    { devEsp :: FilePath
    , devRootfs :: FilePath
    , partUuidEsp :: UUID
    , partUuidRootfs :: UUID
    } deriving (Show)

getInstallDiskInfo :: MonadIO m => FilePath -> m InstallDiskInfo
getInstallDiskInfo device = do
    let devEsp = [i|#{device}1|]
        devRootfs = [i|#{device}2|]
    partUuidEsp <- getDevPartUuid devEsp
    partUuidRootfs <- getDevPartUuid devRootfs
    return InstallDiskInfo {..}

getDevUuid :: MonadIO m => FilePath -> m UUID
getDevUuid = getDevUuid' "uuid"

getDevPartUuid :: MonadIO m => FilePath -> m UUID
getDevPartUuid = getDevUuid' "partuuid"

getDevUuid' :: MonadIO m => Text -> FilePath -> m UUID
getDevUuid' field device = do
    (j :: Value) <-
        throwLeft $ eitherDecode' <$>
        readProcessStdout_ (fromString [i|lsblk --json --nodeps -o #{field} #{device}|])
    case j ^?! key "blockdevices" . _Array . _head . key field of
        String s -> maybe (throwString "Cannot parse invalid UUID") return (fromText s)
        _ -> throwString "Device does not have an UUID"

data DiskInfo
    = DiskInfo { dev :: FilePath
               , uuid :: UUID
               , mounted :: Bool }
    | DiskWithPartitionsInfo { dev :: FilePath
                             , partitions :: [PartitionInfo] }
    deriving (Show, Generic)

data PartitionInfo = PartitionInfo
    { partDev :: FilePath
    , partUuid :: UUID
    , partMounted :: Bool
    } deriving (Show, Generic)

getDiskInfo :: MonadIO m => FilePath -> m DiskInfo
getDiskInfo dev = do
    (j :: Value) <-
        throwLeft $ eitherDecode' <$> readProcessStdout_ (fromString [i|lsblk --json -O #{dev}|])
    let disk = j ^?! key "blockdevices" . _Array . _head
    case disk ^? key "children" . _Array . to toList of
        Nothing -> do
            uuid <- getDevUuid dev
            mounted <- isDevMounted dev
            return DiskInfo {..}
        Just children -> do
            partitions <- mapM getPartInfo children
            return DiskWithPartitionsInfo {..}
  where
    getPartInfo p = do
        let partDev = p ^?! key "path" . _String . _Text
        case p ^?! key "partuuid" of
            String s -> do
                partUuid <- maybe (throwString "Cannot parse invalid UUID") return (fromText s)
                partMounted <- isDevMounted partDev
                return PartitionInfo {..}
            _ -> throwString "Partition does not have an UUID"

findDiskDevice :: MonadIO m => Text -> m FilePath
findDiskDevice model = do
    (j :: Value) <-
        throwLeft $ eitherDecode' <$> readProcessStdout_ "lsblk --json --nodeps -o path,model"
    case j ^?! key "blockdevices" . _Array ^.. folded .
         filtered (\d -> d ^? key "model" . _String == Just model) of
        [d] -> return $ d ^?! key "path" . _String . _Text . to ("/dev" </>)
        [] -> throwString "Device not found"
        _ -> throwString "Could not uniquely identify the device"

isDevMounted :: MonadIO m => FilePath -> m Bool
isDevMounted device = do
    (j :: Value) <-
        throwLeft $ eitherDecode' <$> readProcessStdout_ "findmnt --json --real -o source"
    return . not . null $ j ^?! key "filesystems" . _Array ^.. folded .
        filtered (\fs -> fs ^? key "source" . _String . _Text == Just device)

isDiskMounted :: MonadIO m => FilePath -> m Bool
isDiskMounted device = do
    info <- getDiskInfo device
    return $
        case info of
            DiskInfo {mounted} -> mounted
            DiskWithPartitionsInfo {partitions} -> any partMounted partitions
