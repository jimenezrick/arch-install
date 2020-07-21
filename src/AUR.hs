{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module AUR where

import Command
import Data.String.Interpolate
import RIO
import RIO.FilePath
import RIO.Process
import RIO.Map (insert)
import RIO.Text (pack)

buildAURPackage :: (MonadUnliftIO m, MonadReader env m, HasProcessContext env) => FilePath -> String -> m ()
buildAURPackage dest pkg = do
  withSystemTempDirectory "arch-rebuild-makepkg-" \tmp -> do
    withWorkingDir tmp do
      fetchAURPackage pkg
    withWorkingDir (tmp </> pkg) do
      withModifyEnvVars (insert "PKGDEST" $ pack dest) do
        runCmd_ "makepkg --syncdeps --rmdeps"

installAURPackage :: (MonadUnliftIO m, MonadReader env m, HasProcessContext env) => FilePath -> m ()
installAURPackage path = runCmd_ [i|pacman -U #{path}|]

fetchAURPackage :: (MonadUnliftIO m, MonadReader env m, HasProcessContext env) => String -> m ()
fetchAURPackage pkg = do
  runCmd_ [i|curl -L #{url} | tar zxf -|]
  where
    url = [i|https://aur.archlinux.org/cgit/aur.git/snapshot/#{pkg}.tar.gz|]
