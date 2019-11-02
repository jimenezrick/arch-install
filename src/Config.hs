{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Config where

import RIO
import RIO.Process
import RIO.Time

import Control.Lens
import Data.Binary
import Data.String.Conversions (cs)
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

data PacmanConfig = PacmanConfig
    { _mirrorlist :: Text
    , _packages :: [Text]
    , _groups :: [Text]
    , _aurPackages :: [Text]
    } deriving (Show, Generic)

instance Interpret PacmanConfig

instance Binary PacmanConfig

makeLenses ''PacmanConfig

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

data BuildInfo = BuildInfo
    { _builder :: Text
    , _timestamp :: Text
    , _systemConfig :: SystemConfig
    } deriving (Show, Generic)

instance Binary BuildInfo

makeLenses ''BuildInfo

type LoadedSystemConfig = Text -> Text -> SystemConfig

temporarySystemConfig :: LoadedSystemConfig -> SystemConfig
temporarySystemConfig loadedSysConf = loadedSysConf undefined undefined

auto' :: Interpret a => Type a
auto' = autoWith (defaultInterpretOptions {fieldModifier = T.dropWhile (== '_')})

loadSystemConfig :: MonadIO m => FilePath -> m LoadedSystemConfig
loadSystemConfig path'
    | isURI path' = liftIO $ input auto' $ T.pack path'
    | otherwise = liftIO $ inputFile auto' path'

getBuildInfo :: MonadIO m => SystemConfig -> m BuildInfo
getBuildInfo sysConf = do
    _builder <- cs <$> readProcessStdout_ "hostnamectl status"
    _timestamp <- cs . show <$> liftIO getCurrentTime
    return BuildInfo {_systemConfig = sysConf, ..}

loadBuildInfo :: MonadIO m => FilePath -> m BuildInfo
loadBuildInfo path' = decode . fromStrictBytes <$> readFileBinary path'

saveBuildInfo :: MonadIO m => FilePath -> BuildInfo -> m ()
saveBuildInfo path' buildInfo = writeFileBinary path' . toStrictBytes $ encode buildInfo
