{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}

module Fstab where

import RIO hiding (unlines)
import RIO.Text (pack, unlines)

import Data.String.Interpolate

import Config
import Disk

renderFstab :: MonadIO m => [FstabEntry] -> m (Either Text Text)
renderFstab entries = do
    r <- sequence <$> mapM renderDev entries
    return $ unlines . map pack . concat <$> r
  where
    renderDev entry
        | Disk model <- entry ^. fsEntry = do
            info <- getDiskInfo model
            let render d =
                    [ [i|# #{model}|]
                    , [i|UUID=#{uuid d} #{entry^.fsMountPoint} #{entry^.fsType} #{entry^.fsOpts} #{entry^.fsDump} #{entry^.fsck}|]
                    ]
            return $ render <$> info
        | Device path <- entry ^. fsEntry = do
            return $
                Right
                    [ [i|#{path} #{entry^.fsMountPoint} #{entry^.fsType} #{entry^.fsOpts} #{entry^.fsDump} #{entry^.fsck}|]
                    ]
