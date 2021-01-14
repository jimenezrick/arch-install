{-# LANGUAGE TemplateHaskell #-}

module Version where

import RIO

import GitHash

version :: Either String GitInfo
version = $$tGitInfoCwdTry
