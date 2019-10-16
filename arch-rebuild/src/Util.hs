{-# LANGUAGE NoImplicitPrelude #-}

module Util where

import RIO
import qualified RIO.FilePath as F

(</>) :: FilePath -> FilePath -> FilePath
(</>) a b
    | F.isRelative b = a F.</> b
    | otherwise = a F.</> F.makeRelative "/" b
