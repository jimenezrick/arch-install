{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Config where

import RIO

import Control.Lens
import Dhall

import qualified Data.UUID as U
import qualified RIO.Text as T

data BlockDev
    = DevPath { _path :: FilePath }
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
    , _fstabEntries :: [FstabEntry]
    , _pacman :: PacmanConfig
    } deriving (Show, Generic)

instance Interpret SystemConfig

makeLenses ''SystemConfig

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

data InstallConfig = InstallConfig
    { _system :: SystemConfig
    , _boot :: Text -> BootConfig
    , _espImage :: FilePath
    , _rootfsImage :: FilePath
    , _espImageSize :: Text
    , _rootfsImageSize :: Text
    } deriving (Generic)

instance Interpret InstallConfig

makeLenses ''InstallConfig

auto' :: Interpret a => Type a
auto' = autoWith (defaultInterpretOptions {fieldModifier = T.dropWhile (== '_')})

loadInstallConfig :: MonadIO m => FilePath -> m InstallConfig
loadInstallConfig path' = liftIO $ inputFile auto' path'

loadSystemConfig :: MonadIO m => FilePath -> m SystemConfig
loadSystemConfig path' = liftIO $ inputFile auto' path'

loadBootConfig :: MonadIO m => FilePath -> U.UUID -> m BootConfig
loadBootConfig path' luksUuid = do
    conf <- liftIO $ inputFile auto' path'
    return . conf $ U.toText luksUuid
