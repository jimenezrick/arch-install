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
runCmd_ c = runCmds_ [c]

readCmdOneLine_ :: MonadIO m => String -> m Text
readCmdOneLine_ c = head . lines . cs <$> readProcessStdout_ (fromString c)
