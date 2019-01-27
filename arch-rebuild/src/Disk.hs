{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Disk where

import RIO hiding (to, words)
import RIO.FilePath
import RIO.Process

import Control.Error ((??))
import Control.Lens hiding ((??), children)
import Control.Monad.Except
import Data.Aeson (Value(..), eitherDecode')
import Data.Aeson.Lens
import Data.String.Interpolate
import Data.Text.Lens (_Text)
import Data.UUID (UUID, fromText)

data InstallDiskInfo = InstallDiskInfo
    { devEsp :: FilePath
    , devRootfs :: FilePath
    , uuidEsp :: UUID
    , uuidRootfs :: UUID
    } deriving (Show)

getInstallDiskInfo :: MonadIO m => FilePath -> ExceptT String m InstallDiskInfo
getInstallDiskInfo device = do
    let devEsp = [i|#{device}1|]
        devRootfs = [i|#{device}2|]
    uuidEsp <- getDevUuid devEsp
    uuidRootfs <- getDevUuid devRootfs
    return InstallDiskInfo {..}

getDevUuid :: MonadIO m => FilePath -> ExceptT String m UUID
getDevUuid device = do
    (j :: Value) <-
        ExceptT $ eitherDecode' <$>
        readProcessStdout_ (fromString [i|lsblk --json --nodeps -o uuid #{device}|])
    case j ^?! key "blockdevices" . _Array . _head . key "uuid" of
        String s -> fromText s ?? "Cannot parse invalid UUID"
        _ -> throwError "Device does not have an UUID"

data DiskInfo
    = DiskInfo { dev :: FilePath
               , uuid :: UUID
               , mounted :: Bool }
    | DiskWithPartitionsInfo { dev :: FilePath
                             , partitions :: [PartitionInfo] }
    deriving (Show)

data PartitionInfo = PartitionInfo
    { partDev :: FilePath
    , partUuid :: UUID
    , partMounted :: Bool
    } deriving (Show)

getDiskInfo :: MonadIO m => Text -> ExceptT String m DiskInfo
getDiskInfo model = do
    dev <- findDiskDevice model
    (j :: Value) <-
        ExceptT $ eitherDecode' <$> readProcessStdout_ (fromString [i|lsblk --json -O #{dev}|])
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
                partUuid <- fromText s ?? "Cannot parse invalid UUID"
                partMounted <- isDiskMounted partDev
                return PartitionInfo {..}
            _ -> throwError "Partition does not have an UUID"

findDiskDevice :: MonadIO m => Text -> ExceptT String m FilePath
findDiskDevice model = do
    (j :: Value) <-
        ExceptT $ eitherDecode' <$> readProcessStdout_ "lsblk --json --nodeps -o path,model"
    case j ^?! key "blockdevices" . _Array ^.. folded .
         filtered (\d -> d ^?! key "model" . _String == model) of
        [d] -> return $ d ^?! key "path" . _String . _Text . to ("/dev" </>)
        [] -> throwError "Device not found"
        _ -> throwError "Could not uniquely identify the device"

isDiskMounted :: MonadIO m => FilePath -> ExceptT String m Bool
isDiskMounted device = do
    (j :: Value) <- ExceptT $ eitherDecode' <$> readProcessStdout_ "findmnt --json --real -o source"
    return . not . null $ j ^?! key "filesystems" . _Array ^.. folded .
        filtered (\fs -> fs ^?! key "source" . _String . _Text == device)
