{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Disk where

import RIO hiding (words)
import RIO.FilePath
import RIO.List.Partial (head)
import RIO.Process
import RIO.Text (Text, unpack, words)

import Data.String.Interpolate

import Command
import Match

data InstallDiskInfo = InstallDiskInfo
    { devEsp :: FilePath
    , devRootfs :: FilePath
    , uuidEsp :: Text
    , uuidRootfs :: Text
    } deriving (Show)

getInstallDiskInfo :: MonadIO m => FilePath -> m InstallDiskInfo
getInstallDiskInfo device = do
    let devEsp = [i|#{device}1|]
        devRootfs = [i|#{device}2|]
    uuidEsp <- getDevUuid devEsp
    uuidRootfs <- getDevUuid devRootfs
    return InstallDiskInfo {..}

getDevUuid :: MonadIO m => FilePath -> m Text
getDevUuid device = readCmdOneLine_ [i|lsblk -n --nodeps -o uuid #{device}|]

data DiskInfo = DiskInfo
    { dev :: FilePath
    , uuid :: Text
    , mounted :: Bool
    } deriving (Show)

-- TODO: Check somewhere else it is not mounted
getDiskInfo :: MonadIO m => Text -> m (Either Text DiskInfo)
getDiskInfo model = do
    disk <- findDiskDevice model
    case disk of
        Left err -> return $ Left err
        Right dev -> do
            uuid <- getDevUuid dev
            mounted <- isDiskMounted dev
            return $ Right DiskInfo {..}

findDiskDevice :: MonadIO m => Text -> m (Either Text FilePath)
findDiskDevice model = do
    disks <-
        linesMatchingExactWords [model] <$>
        readProcessStdout_ "lsblk -n --nodeps --scsi -o kname,model"
    return $
        case disks of
            [d] -> Right . ("/dev" </>) . unpack . head $ words d
            [] -> Left "disk not found"
            _ -> Left "could not uniquely identify the disk"

isDiskMounted :: MonadIO m => FilePath -> m Bool
isDiskMounted device =
    not . null . linesMatchingExactWords [device] <$>
    readProcessStdout_ "findmnt -n --real -o source"
