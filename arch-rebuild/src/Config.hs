{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Config where

import RIO

import Dhall
import Lens.Micro.Platform

import qualified RIO.Text as T

data FstabEntryType
    = Disk { _model :: Text }
    | Device { _path :: FilePath }
    deriving (Show, Generic)

instance Interpret FstabEntryType

makeLenses ''FstabEntryType

data FstabEntry = FstabEntry
    { _fsEntry :: FstabEntryType
    , _fsMountPoint :: FilePath
    , _fsType :: Text
    , _fsOpts :: Text
    , _fsDump :: Natural
    , _fsck :: Natural
    } deriving (Show, Generic)

instance Interpret FstabEntry

makeLenses ''FstabEntry

data SystemConfig = SystemConfig
    { _rootDiskModel :: Text
    , _zoneInfo :: Text
    , _locale :: Text
    , _keymap :: Text
    , _hostname :: Text
    , _fstabEntries :: [FstabEntry]
    , _pacmanMirrorlist :: Text
    , _pacmanExplicitPackages :: [Text]
    , _pacmanPackageGroups :: [Text]
    , _pacmanAurPackages :: [Text]
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

auto' :: Interpret a => Type a
auto' = autoWith (defaultInterpretOptions {fieldModifier = T.dropWhile (== '_')})

loadSystemConfig :: MonadIO m => FilePath -> m SystemConfig
loadSystemConfig path' = liftIO $ inputFile auto' path'

loadBootConfig :: MonadIO m => Text -> Text -> FilePath -> m BootConfig
loadBootConfig luksName luksUuid path' = do
    conf <- liftIO $ inputFile auto' path'
    return $ conf luksName luksUuid
