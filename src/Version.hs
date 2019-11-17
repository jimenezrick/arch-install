{-# LANGUAGE TemplateHaskell #-}

module Version where

import GitHash

version :: GitInfo
version = $$tGitInfoCwd
