{-# LANGUAGE QuasiQuotes #-}

module AUR where

import Command
import Data.String.Interpolate
import RIO
import RIO.FilePath
import RIO.Process

buildAURPackage :: (MonadUnliftIO m, MonadReader env m, HasProcessContext env) => Bool -> FilePath -> m ()
buildAURPackage install path = do
  withWorkingDir path do
    runCmd_ [i|makepkg #{unwords flags}|]
  where
    flags = ["--syncdeps", "--rmdeps"] ++ if install then ["--install"] else []

installAURPackage :: (MonadUnliftIO m, MonadReader env m, HasProcessContext env) => String -> m ()
installAURPackage pkg = do
  withSystemTempDirectory "arch-rebuild-aur-" \dir -> do
    withWorkingDir dir do
      fetchAURPackage pkg
    buildAURPackage True $ dir </> pkg

fetchAURPackage :: (MonadUnliftIO m, MonadReader env m, HasProcessContext env) => String -> m ()
fetchAURPackage pkg = do
  runCmd_ [i|curl #{url} | tar zxf -|]
  where
    url = [i|https://aur.archlinux.org/cgit/aur.git/snapshot/#{pkg}.tar.gz|]

installAuracle :: (MonadUnliftIO m, MonadReader env m, HasProcessContext env) => m ()
installAuracle = installAURPackage "auracle-git"
