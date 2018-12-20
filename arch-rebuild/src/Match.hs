{-# LANGUAGE NoImplicitPrelude #-}

module Match where

import RIO hiding (lines)
import RIO.Text (Text, lines, pack)

import qualified Data.Attoparsec.Combinator as A
import qualified Data.Attoparsec.Text as A

linesMatchingWords :: [Text] -> Text -> [Text]
linesMatchingWords words' txt = rights . map (A.parseOnly (matchWords words')) $ lines txt

matchWords :: [Text] -> A.Parser Text
matchWords words' = mconcat <$> traverse (\w -> mappend <$> skipBefore w <*> A.string w) words'

skipBefore :: Text -> A.Parser Text
skipBefore word = pack <$> (A.manyTill A.anyChar . A.lookAhead $ A.string word)
