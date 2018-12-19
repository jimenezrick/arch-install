{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module Command where

import RIO hiding (lines)
import RIO.List.Partial (head)
import RIO.Process
import RIO.Text (Text, lines)

import Data.String.Conversions (cs)

runCommands_ :: MonadIO m => [String] -> m ()
runCommands_ = mapM_ (runProcess_ . fromString)

runCommand_ :: MonadIO m => String -> m ()
runCommand_ c = runCommands_ [c]

readCommandStdoutOneLine_ :: MonadIO m => String -> m Text
readCommandStdoutOneLine_ c = head . lines . cs <$> readProcessStdout_ (fromString c)
