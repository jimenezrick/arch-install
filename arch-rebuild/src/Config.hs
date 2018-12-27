{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Config where

import RIO

import Dhall
import Lens.Micro.Platform

data FstabEntryType
    = Disk { model :: Text }
    | Device { path :: FilePath }
    deriving (Show, Generic)

instance Interpret FstabEntryType

makeLenses ''FstabEntryType

data FstabEntry = FstabEntry
    { fsEntry :: FstabEntryType
    , fsMountPoint :: FilePath
    , fsType :: Text
    , fsOpts :: Text
    , fsDump :: Natural
    , fsck :: Natural
    } deriving (Show, Generic)

instance Interpret FstabEntry

makeLenses ''FstabEntry

data SystemConfig = SystemConfig
    { rootDiskModel :: Text
    , zoneInfo :: Text
    , locale :: Text
    , keymap :: Text
    , hostname :: Text
    , fstabEntries :: [FstabEntry]
    , pacmanMirrorlist :: Text
    , pacmanExplicitPackages :: [Text]
    , pacmanPackageGroups :: [Text]
    , pacmanAurPackages :: [Text]
    } deriving (Show, Generic)

instance Interpret SystemConfig

makeLenses ''SystemConfig

data BootEntries = BootEntries
    { bootName :: Text
    , bootConf :: Text
    } deriving (Show, Generic)

instance Interpret BootEntries

makeLenses ''BootEntries

data BootConfig = BootConfig
    { loaderConf :: Text
    , entries :: [BootEntries]
    } deriving (Show, Generic)

instance Interpret BootConfig

makeLenses ''BootConfig

loadSystemConfig :: MonadIO m => FilePath -> m SystemConfig
loadSystemConfig path' = liftIO $ inputFile auto path'

loadBootConfig :: MonadIO m => Text -> Text -> FilePath -> m BootConfig
loadBootConfig luksName luksUuid path' = do
    conf <- liftIO $ inputFile auto path'
    return $ conf luksName luksUuid
