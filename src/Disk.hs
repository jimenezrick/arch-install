{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
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

import Config
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

getDevModel :: MonadIO m => FilePath -> m (Maybe Text)
getDevModel device = do
    f <- getDevField "model" device
    case f of
        String model -> return $ Just model
        _ -> return Nothing

getDevUuidIfValid :: MonadUnliftIO m => FilePath -> m (Maybe UUID)
getDevUuidIfValid device = catch (getDevUuid device) (\(_ :: StringException) -> return Nothing)

getDevUuid :: MonadIO m => FilePath -> m (Maybe UUID)
getDevUuid device = do
    f <- getDevField "uuid" device
    case f of
        String s -> maybe (throwString "Cannot parse invalid UUID") (return . Just) (fromText s)
        _ -> return Nothing

getDevPartUuid :: MonadIO m => FilePath -> m UUID
getDevPartUuid partDev = do
    f <- getDevField "partuuid" partDev
    case f of
        String s -> maybe (throwString "Cannot parse invalid UUID") return (fromText s)
        _ -> throwString "Invalid partition device"

getDevField :: MonadIO m => Text -> FilePath -> m Value
getDevField field device = do
    (j :: Value) <-
        throwLeft $ eitherDecode' <$>
        readProcessStdout_ (fromString [i|lsblk --json --nodeps -o #{field} #{device}|])
    return $ j ^?! key "blockdevices" . _Array . _head . key field

data DiskInfo
    = DiskInfo { dev :: FilePath
               , model :: Maybe Text
               , uuid :: Maybe UUID
               , mounted :: Bool }
    | DiskWithPartitionsInfo { dev :: FilePath
                             , model :: Maybe Text
                             , partitions :: [PartitionInfo] }
    deriving (Show, Generic)

data PartitionInfo = PartitionInfo
    { partDev :: FilePath
    , uuid :: Maybe UUID
    , partUuid :: UUID
    , partMounted :: Bool
    } deriving (Show, Generic)

getDiskInfo :: MonadUnliftIO m => BlockDev -> m DiskInfo
getDiskInfo blockdev = do
    dev <- findDiskDevice blockdev
    model <- getDevModel dev
    (j :: Value) <-
        throwLeft $ eitherDecode' <$> readProcessStdout_ (fromString [i|lsblk --json -O #{dev}|])
    let disk = j ^?! key "blockdevices" . _Array . _head
    case disk ^? key "children" . _Array . to toList of
        Nothing -> do
            uuid <- getDevUuidIfValid dev
            mounted <- isDevMounted dev
            return DiskInfo {..}
        Just children -> do
            partitions <- mapM getPartInfo children
            return DiskWithPartitionsInfo {..}
  where
    getPartInfo p = do
        let partDev = p ^?! key "path" . _String . _Text
        uuid <- getDevUuidIfValid partDev
        partUuid <- getDevPartUuid partDev
        partMounted <- isDevMounted partDev
        return PartitionInfo {..}

findDiskDevice :: MonadIO m => BlockDev -> m FilePath
findDiskDevice (DevPath path) = return path
findDiskDevice (DiskModel model) = findDiskModelDevice model
findDiskDevice (DiskModelPartition model partNum) = do
    dev <- findDiskModelDevice model
    return [i|#{dev}#{partNum}|]
findDiskDevice _ = throwString "Not implemented" -- TODO: FsUUID, PartUUID

findDiskModelDevice :: MonadIO m => Text -> m FilePath
findDiskModelDevice model = do
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

isDiskMounted :: MonadUnliftIO m => BlockDev -> m Bool
isDiskMounted blockdev = do
    info <- getDiskInfo blockdev
    return $
        case info of
            DiskInfo {mounted} -> mounted
            DiskWithPartitionsInfo {partitions} -> any partMounted partitions
