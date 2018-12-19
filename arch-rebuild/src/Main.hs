{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Main where

import Prelude (putStrLn)
import RIO

import Labels
import Options.Generic

import Install

newtype CmdOpts = Install
    { confPath :: FilePath
    } deriving (Generic)

instance ParseRecord CmdOpts

main :: IO ()
main = do
    cmd <- getRecord "foo"
    case cmd of
        Install confPath -> runSimpleApp (doPreInstallChecks >> installArch device)
        _ -> putStrLn "Nothing to do here"
  where
    device = "/dev/sda" -- XXX

--
-- XXX: Experiment
--
experiment :: Has "foo" value record => record -> value
experiment = get #foo
