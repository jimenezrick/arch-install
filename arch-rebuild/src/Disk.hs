{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
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

import Command
import Error

data InstallDiskInfo = InstallDiskInfo
    { devEsp :: FilePath
    , devRootfs :: FilePath
    , uuidEsp :: UUID
    , uuidRootfs :: UUID
    } deriving (Show)

getInstallDiskInfo :: MonadIO m => FilePath -> m InstallDiskInfo
getInstallDiskInfo device = do
    let devEsp = [i|#{device}1|]
        devRootfs = [i|#{device}2|]
    uuidEsp <- getDevUuid devEsp
    uuidRootfs <- getDevUuid devRootfs
    return InstallDiskInfo {..}

getDevUuid :: MonadIO m => FilePath -> m UUID
getDevUuid device = do
    (j :: Value) <-
        throwLeft $ eitherDecode' <$>
        readProcessStdout_ (fromString [i|lsblk --json --nodeps -o uuid #{device}|])
    case j ^?! key "blockdevices" . _Array . _head . key "uuid" of
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

getDiskInfo :: MonadIO m => Text -> m DiskInfo
getDiskInfo model = do
    dev <- findDiskDevice model
    (j :: Value) <-
        throwLeft $ eitherDecode' <$> readProcessStdout_ (fromString [i|lsblk --json -O #{dev}|])
    let disk = j ^?! key "blockdevices" . _Array . _head
    case disk ^? key "children" . _Array . to toList of
        Nothing -> do
            uuid <- getDevUuid dev
            mounted <- isDiskMounted dev
            return DiskInfo {..}
        Just children -> do
            partitions <- mapM getPartInfo children
            return DiskWithPartitionsInfo {..}
  where
    getPartInfo p = do
        let partDev = p ^?! key "path" . _String . _Text
        case p ^?! key "uuid" of
            String s -> do
                partUuid <- maybe (throwString "Cannot parse invalid UUID") return (fromText s)
                partMounted <- isDiskMounted partDev
                return PartitionInfo {..}
            _ -> throwString "Partition does not have an UUID"

findDiskDevice :: MonadIO m => Text -> m FilePath
findDiskDevice model = do
    (j :: Value) <-
        throwLeft $ eitherDecode' <$> readProcessStdout_ "lsblk --json --nodeps -o path,model"
    case j ^?! key "blockdevices" . _Array ^.. folded .
         filtered (\d -> d ^?! key "model" . _String == model) of
        [d] -> return $ d ^?! key "path" . _String . _Text . to ("/dev" </>)
        [] -> throwString "Device not found"
        _ -> throwString "Could not uniquely identify the device"

isDiskMounted :: MonadIO m => FilePath -> m Bool
isDiskMounted device = do
    (j :: Value) <-
        throwLeft $ eitherDecode' <$> readProcessStdout_ "findmnt --json --real -o source"
    return . not . null $ j ^?! key "filesystems" . _Array ^.. folded .
        filtered (\fs -> fs ^?! key "source" . _String . _Text == device)

createZeroImage :: MonadIO m => FilePath -> Int -> m ()
createZeroImage path sizeMegas = runCmd_ [i|dd if=/dev/zero of=#{path} bs=1M count=#{sizeMegas}|]

mountLoopImage :: MonadIO m => FilePath -> FilePath -> m ()
mountLoopImage imgPath mntPoint = runCmd_ [i|mount -o loop #{imgPath} #{mntPoint}|]
