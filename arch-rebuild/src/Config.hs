{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Config where

import RIO

import Control.Lens
import Dhall

import qualified RIO.Text as T

data BlockDev
    = FsUUID { _uuid :: Text }
    | DevPath { _path :: FilePath }
    | DiskModel { _model :: Text }
    | Partition { _diskModel :: Text
                , _partNum :: Natural }
    deriving (Show, Generic)

instance Interpret BlockDev

makeLenses ''BlockDev

data FstabEntry = FstabEntry
    { _fsEntry :: BlockDev
    , _fsMountPoint :: FilePath
    , _fsType :: Text
    , _fsOpts :: Text
    , _fsDump :: Natural
    , _fsck :: Natural
    } deriving (Show, Generic)

instance Interpret FstabEntry

makeLenses ''FstabEntry

data BootEntries = BootEntries
    { _bootName :: Text
    , _bootConf :: Text
    } deriving (Show, Generic)

instance Interpret BootEntries

makeLenses ''BootEntries

data BootConfig = BootConfig
    { _loaderConf :: Text
    , _bootEntries :: [BootEntries]
    } deriving (Show, Generic)

instance Interpret BootConfig

makeLenses ''BootConfig

data StorageConfig = StorageConfig
    { _boot :: BootConfig
    , _fstabEntries :: [FstabEntry]
    , _espImage :: FilePath
    , _rootfsImage :: FilePath
    , _espImageSize :: Text
    , _rootfsImageSize :: Text
    , _rootSubvolumes :: [(String, FilePath)]
    } deriving (Show, Generic)

instance Interpret StorageConfig

makeLenses ''StorageConfig

data PacmanConfig = PacmanConfig
    { _mirrorlist :: Text
    , _explicitPackages :: [Text]
    , _packageGroups :: [Text]
    , _aurPackages :: [Text]
    } deriving (Show, Generic)

instance Interpret PacmanConfig

makeLenses ''PacmanConfig

data SystemConfig = SystemConfig
    { _hostname :: Text
    , _zoneInfo :: Text
    , _locale :: Text
    , _keymap :: Text
    , _storage :: StorageConfig
    , _packages :: PacmanConfig
    } deriving (Show, Generic)

instance Interpret SystemConfig

makeLenses ''SystemConfig

auto' :: Interpret a => Type a
auto' = autoWith (defaultInterpretOptions {fieldModifier = T.dropWhile (== '_')})

loadSystemConfig :: MonadIO m => FilePath -> m SystemConfig
loadSystemConfig path' = liftIO $ inputFile auto' path'
