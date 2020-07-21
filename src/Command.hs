module Command where

import RIO hiding (lines)
import RIO.List.Partial (head)
import RIO.Process
import RIO.Text (lines)

import Data.String.Conversions (cs)
import System.Process.Typed (setWorkingDir, setEnv)

runCmds_ :: (MonadIO m, MonadReader env m, HasProcessContext env) => [String] -> m ()
runCmds_ cmds = do
    ev <- view envVarsStringsL
    wd <- view workingDirL
    mapM_ (runProcess_ . toProcessConfig ev wd) cmds

runCmd_ :: (MonadIO m, MonadReader env m, HasProcessContext env) => String -> m ()
runCmd_ cmd = runCmds_ [cmd]

toProcessConfig :: [(String, String)] -> Maybe FilePath -> String -> ProcessConfig () () ()
toProcessConfig ev wd = setEnv ev . maybe id setWorkingDir wd . fromString

readCmdOneLine_ :: MonadIO m => String -> m Text
readCmdOneLine_ cmd = head . lines . cs <$> readProcessStdout_ (fromString cmd)
