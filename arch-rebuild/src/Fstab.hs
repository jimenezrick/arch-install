{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}

module Fstab where

import RIO hiding (unlines)
import RIO.Text (unlines)

import Data.String.Interpolate

import Config

renderFstab :: MonadIO m => [FstabEntry] -> m Text
renderFstab =
    return .
    unlines .
    map (\e ->
             fromString
                 [i|#{renderDev $ e^.fsEntry} #{e^.fsMountPoint} #{e^.fsType} #{e^.fsOpts} #{e^.fsDump} #{e^.fsck}|])
  where
    renderDev (Disk model) = [i|# #{model}|]
    renderDev (Device path) = [path]
