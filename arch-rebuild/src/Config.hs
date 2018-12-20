{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Config where

import RIO

import Dhall
import Lens.Micro.Platform

data FstabEntry
    = DiskModel Text
    | Device FilePath
    deriving (Show, Generic)

instance Interpret FstabEntry

makeLenses ''FstabEntry

data FstabConfig = FstabConfig
    { fstanEntry :: FstabEntry
    , mountPoint :: FilePath
    , fsType :: Text
    , fsOpts :: Text
    , dump :: Natural
    , fsck :: Natural
    } deriving (Show, Generic)

instance Interpret FstabConfig

data SystemConfig = SystemConfig
    { rootDiskModel :: Text
    , zoneInfo :: Text
    , locale :: Text
    , keymap :: Text
    , hostname :: Text
    , fstab :: FstabConfig
    , pacmanMirrorlist :: Text
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
loadSystemConfig path = liftIO $ inputFile auto path

loadBootConfig :: MonadIO m => Text -> Text -> FilePath -> m BootConfig
loadBootConfig luksName luksUuid path = do
    conf <- liftIO $ inputFile auto path
    return $ conf luksName luksUuid
