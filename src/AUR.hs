{-# LANGUAGE QuasiQuotes #-}

module AUR where

import RIO
import RIO.Process
import RIO.Text (unpack)

import Data.String.Interpolate

import Command

buildAurPackage :: (MonadUnliftIO m, MonadReader env m, HasProcessContext env) => Bool -> String -> m ()
buildAurPackage install pkg = do
    installAuracle -- XXX: only if not installed
    doesExecutableExist "caca" >>= traceShowIO
    runCmd_ [i|caca2|]
    -- runCmd_ [i|makepkg -a #{pkg}|]
        {-
        --syncdeps
        --install
        --rmdeps
        --asdeps

-}

installAuracle :: (MonadUnliftIO m, MonadReader env m, HasProcessContext env) => m ()
installAuracle = do
    withSystemTempDirectory "aur-build-" $ \d -> do
        traceShowIO d
        withWorkingDir d $ do
            runCmd_ "pwd"
            -- runCmd_ [i|curl #{url} | tar zxf -|]
            -- withWorkingDir "auracle-git" $ runCmd_ "ls"
  where
    url = "https://aur.archlinux.org/cgit/aur.git/snapshot/auracle-git.tar.gz"
