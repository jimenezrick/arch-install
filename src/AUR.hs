{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module AUR where

import RIO
import RIO.Directory (makeAbsolute)
import RIO.FilePath
import RIO.Process
import RIO.Map (insert)
import RIO.Text (pack)

import Data.String.Interpolate

import Command

buildAURPackage :: (MonadUnliftIO m, MonadReader env m, HasProcessContext env) => FilePath -> String -> m ()
buildAURPackage dest pkg = do
  withSystemTempDirectory "arch-rebuild-makepkg-" \tmp -> do
    withWorkingDir tmp do
      fetchAURPackage pkg
    withWorkingDir (tmp </> pkg) do
      absDest <- makeAbsolute dest
      withModifyEnvVars (insert "PKGDEST" $ pack absDest) do
        runCmd_ "makepkg --syncdeps --rmdeps"

installAURPackage :: (MonadUnliftIO m, MonadReader env m, HasProcessContext env) => FilePath -> FilePath -> m ()
installAURPackage rootfs path = runCmd_ [i|pacman --sysroot #{rootfs} -U #{path}|]

fetchAURPackage :: (MonadUnliftIO m, MonadReader env m, HasProcessContext env) => String -> m ()
fetchAURPackage pkg = do
  runCmd_ [i|curl -L #{url} | tar zxf -|]
  where
    url = [i|https://aur.archlinux.org/cgit/aur.git/snapshot/#{pkg}.tar.gz|]
