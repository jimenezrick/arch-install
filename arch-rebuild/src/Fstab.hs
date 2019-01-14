{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}

module Fstab where

import RIO hiding (unlines)
import RIO.Text (pack, unlines)

import Data.String.Interpolate

import Config

renderFstab :: MonadIO m => [FstabEntry] -> m Text
renderFstab = fmap (unlines . map pack . concat) . mapM renderDev
  where
    renderDev e
        -- | (Disk model) <- e ^. fsEntry = [i|# #{model}|] [i|UUID=#{...}|]
        | Device path <- e ^. fsEntry =
            return
                [[i|#{path} #{e^.fsMountPoint} #{e^.fsType} #{e^.fsOpts} #{e^.fsDump} #{e^.fsck}|]]
