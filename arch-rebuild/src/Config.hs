{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Config where

import RIO

import Dhall
import Lens.Micro.Platform

data SystemConfig = SystemConfig
    { diskModel :: Text
    , zoneinfo :: Text
    , locale :: Text
    , keymap :: Text
    , hostname :: Text
    , pacmanMirrorlist :: Text
    , pacmanPackageGroups :: [Text]
    , pacmanAurPackages :: [Text]
    } deriving (Show, Generic)

instance Interpret SystemConfig

makeLenses ''SystemConfig

data BootEntries = BootEntries
    { name :: Text
    , conf :: Text
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
