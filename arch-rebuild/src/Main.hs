{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Prelude (putStrLn)

import RIO hiding (lines, threadDelay)
import RIO.ByteString.Lazy (toStrict)
import RIO.Directory
import RIO.List.Partial (head)
import RIO.Prelude.Simple
import RIO.Process
import RIO.Text (Text, lines, pack)

import Data.String.Interpolate
import System.Exit (exitFailure)
import System.Posix.User (getEffectiveUserID)
import Time.Units (Second, Time(..), threadDelay)

import qualified Data.Attoparsec.Combinator as A
import qualified Data.Attoparsec.Text as A
import qualified Data.Either

type App = RIO SimpleApp

main :: IO ()
main = runSimpleApp (doPreInstallChecks >> installArch device)
  where
    device = "/dev/sda" -- XXX

doPreInstallChecks :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env) => m ()
doPreInstallChecks = do
    amIRoot >>= exitIfNot (logError "must be run as root")
    isUefiSystem >>= exitIfNot (logError "not booted in UEFI mode")
    isNetworkReady >>= exitIfNot (logError "network not ready")
    isClockSynced >>= exitIfNot (logError "clock not in sync")
  where
    exitIfNot f b = unless b f >> liftIO exitFailure

--
-- Utilities
--
amIRoot :: MonadIO m => m Bool
amIRoot = (== 0) <$> liftIO getEffectiveUserID

isUefiSystem :: MonadIO m => m Bool
isUefiSystem = doesDirectoryExist "/sys/firmware/efi"

isNetworkReady :: MonadUnliftIO m => m Bool
isNetworkReady =
    catch
        (readProcess "ping -c1 archlinux.org" >> return True)
        (\(_ :: ExitCodeException) -> return False)

isClockSynced :: MonadIO m => m Bool
isClockSynced = do
    runProcess_ "timedatectl set-ntp true"
    threadDelay (Time @Second 2)
    readProcessStdout_ "timedatectl status" >>=
        return . isJust . matchWordsAnyLine ["synchronized", "no"] . toText
  where
    toText = decodeUtf8Lenient . toStrict

--
-- Parsing
--
matchWordsAnyLine :: [Text] -> Text -> Maybe Text
matchWordsAnyLine words txt =
    let matches = rights $ map (A.parseOnly (matchWords words)) $ lines txt
     in case matches of
            [] -> Nothing
            m:_ -> Just m

matchWords :: [Text] -> A.Parser Text
matchWords words = mconcat <$> traverse (\w -> mappend <$> skipBefore w <*> A.string w) words

skipBefore :: Text -> A.Parser Text
skipBefore word = pack <$> (A.manyTill A.anyChar . A.lookAhead $ A.string word)

--
-- Installer
--
installArch :: (MonadIO m, MonadReader env m, HasLogFunc env) => Text -> m ()
installArch device = do
    let devEsp = [i|#{device}1|]
    let devRootfs = [i|#{device}2|]
    esp_uuid <- readCommandStdoutLine_ [i|lsblk -n -o UUID #{devEsp}|]
    rootfs_uuid <- readCommandStdoutLine_ [i|lsblk -n -o UUID #{devRootfs}|]
    let cwd = "XXX" -- XXX
        install_pkgs = ["caca", "pedo"] -- XXX
        install_groups = ["caca", "pedo"] -- XXX
    logInfo "Installing Arch"
    runCommands_
        [ [i|cp -v #{cwd}/mirrorlist /etc/pacman.d/|]
        , [i|pacstrap /mnt base btrfs-progs ${install_pkgs[@]} ${install_groups[@]}|]
        ]
    logInfo "Configuring chroot Arch"
    runCommand_ [i|#{cwd}/fstab.sh #{esp_uuid} #{rootfs_uuid} >>/mnt/etc/fstab|]
  where
    runCommands_ = sequence_ . map (runProcess_ . fromString)
    runCommand_ c = runCommands_ [c]
    readCommandStdoutLine_ c =
        head . lines . decodeUtf8Lenient . toStrict <$> readProcessStdout_ (fromString c)
