{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Disk where

import RIO hiding (words)
import RIO.List.Partial (head)
import RIO.Process
import RIO.Text (Text, words)

import Data.String.Conversions (cs)

import Match

findDiskDevice :: MonadIO m => Text -> m (Either Text Text)
findDiskDevice model = do
    disks <- linesMatchingWords [model] . cs <$> readProcessStdout_ "lsblk -n -S -o kname,model"
    case disks of
        [d] -> do
            let dev = head $ words d
            mounts <-
                linesMatchingWords [dev] . cs <$> readProcessStdout_ "findmnt -n --real -o source"
            if null mounts
                then return $ Right dev
                else return $ Left "disk is currently mounted"
        [] -> return $ Left "disk not found"
        _ -> return $ Left "could not uniquely identify a disk"
