{-# LANGUAGE NoImplicitPrelude #-}

module Command where

import RIO hiding (lines)
import RIO.List.Partial (head)
import RIO.Process
import RIO.Text (Text, lines)

import Data.String.Conversions (cs)

runCmds_ :: MonadIO m => [String] -> m ()
runCmds_ = mapM_ (runProcess_ . fromString)

runCmd_ :: MonadIO m => String -> m ()
runCmd_ cmd = runCmds_ [cmd]

readCmdOneLine_ :: MonadIO m => String -> m Text
readCmdOneLine_ cmd = head . lines . cs <$> readProcessStdout_ (fromString cmd)
