{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module Match where

import RIO hiding (lines)
import RIO.Text (Text, lines, pack)

import Data.String.Conversions

import qualified Data.Attoparsec.Combinator as A
import qualified Data.Attoparsec.Text as A

linesMatchingWords :: (ConvertibleStrings a Text, ConvertibleStrings b Text) => [a] -> b -> [Text]
linesMatchingWords words' txt =
    rights . map (A.parseOnly (matchWords $ map cs words')) . lines $ cs txt

matchWords :: [Text] -> A.Parser Text
matchWords words' = mconcat <$> traverse (\w -> mappend <$> skipBefore w <*> A.string w) words'

skipBefore :: Text -> A.Parser Text
skipBefore word = pack <$> (A.manyTill A.anyChar . A.lookAhead $ A.string word)
