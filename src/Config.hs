{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Config where

import RIO

import Control.Lens
import Data.Binary
import Dhall
import Network.URI (isURI)

import qualified RIO.Text as T

data BlockDev
    = FsUUID { _uuid :: Text }
    | PartUUID { _partUuid :: Text }
    | DevPath { _path :: FilePath }
    | DiskModel { _model :: Text }
    | DiskModelPartition { _diskModel :: Text
                         , _partNum :: Natural }
    deriving (Show, Generic)

instance Interpret BlockDev

instance Binary BlockDev

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

instance Binary FstabEntry

makeLenses ''FstabEntry

data BootEntries = BootEntries
    { _bootName :: Text
    , _bootConf :: Text
    } deriving (Show, Generic)

instance Interpret BootEntries

instance Binary BootEntries

makeLenses ''BootEntries

data BootConfig = BootConfig
    { _loaderConf :: Text
    , _bootEntries :: [BootEntries]
    } deriving (Show, Generic)

instance Interpret BootConfig

instance Binary BootConfig

makeLenses ''BootConfig

data StorageConfig = StorageConfig
    { _rootDisk :: BlockDev
    , _rootSubvolumes :: [(String, FilePath)]
    , _boot :: BootConfig
    , _fstabEntries :: [FstabEntry]
    } deriving (Show, Generic)

instance Interpret StorageConfig

instance Binary StorageConfig

makeLenses ''StorageConfig

-- XXX: Snapshot version (optional)
data PacmanConfig = PacmanConfig
    { _mirrorlist :: Text
    , _packages :: [Text]
    , _groups :: [Text]
    , _aurPackages :: [Text]
    } deriving (Show, Generic)

instance Interpret PacmanConfig

instance Binary PacmanConfig

makeLenses ''PacmanConfig

-- XXX: Wrap this with BuildInfo: uuid, builder, timestamp, package versions
data SystemConfig = SystemConfig
    { _hostname :: Text
    , _zoneInfo :: Text
    , _locale :: Text
    , _keymap :: Text
    , _storage :: StorageConfig
    , _pacman :: PacmanConfig
    } deriving (Show, Generic)

instance Interpret SystemConfig

instance Binary SystemConfig

makeLenses ''SystemConfig

auto' :: Interpret a => Type a
auto' = autoWith (defaultInterpretOptions {fieldModifier = T.dropWhile (== '_')})

loadSystemConfig :: MonadIO m => FilePath -> m SystemConfig
loadSystemConfig path'
    | isURI path' = liftIO $ input auto' $ T.pack path'
    | otherwise = liftIO $ inputFile auto' path'

saveBinSystemConfig :: MonadIO m => FilePath -> SystemConfig -> m ()
saveBinSystemConfig path' sysConf = writeFileBinary path' . toStrictBytes $ encode sysConf

loadBinSystemConfig :: MonadIO m => FilePath -> m SystemConfig
loadBinSystemConfig path' = decode . fromStrictBytes <$> readFileBinary path'