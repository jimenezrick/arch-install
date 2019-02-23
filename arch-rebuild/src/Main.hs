{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Main where

import RIO
import RIO.FilePath ((</>))
import RIO.Text (pack)

import Control.Monad.Except
import Labels
import Options.Generic
import System.Environment (getProgName)

import Config
import Install

data CmdOpts
    = Install { confPath :: FilePath }
    | Other -- XXX
    deriving (Generic)

instance ParseRecord CmdOpts where
    parseRecord = parseRecordWithModifiers $ lispCaseModifiers {shortNameModifier = firstLetter}

main :: IO ()
main = do
    cmd <- pack <$> getProgName >>= getRecord
    case cmd of
        Install confPath -> do
            sysConf <- loadSystemConfig $ confPath </> "system.dhall"
            runSimpleApp $ do
                doPreInstallChecks
                r <- runExceptT $ installArch sysConf
                case r of
                    Left err -> logError $ fromString err
                    Right () -> logInfo "Done"
        _ -> undefined

--
-- XXX: Experiment
--
experiment :: Has "foo" value record => record -> value
experiment = get #foo
