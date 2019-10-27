{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module Match where

import RIO hiding (lines, words)
import RIO.List (isSubsequenceOf)
import RIO.Text (Text, lines, pack, words)

import Data.String.Conversions

import qualified Data.Attoparsec.Combinator as A
import qualified Data.Attoparsec.Text as A

linesMatchingExactWords ::
       (ConvertibleStrings a Text, ConvertibleStrings b Text) => [a] -> b -> [Text]
linesMatchingExactWords words' txt = filter (isSubsequenceOf words'' . words) . lines $ cs txt
  where
    words'' = map cs words'

linesMatchingWords :: (ConvertibleStrings a Text, ConvertibleStrings b Text) => [a] -> b -> [Text]
linesMatchingWords words' txt = filter (isRight . A.parseOnly (matchWords words'')) . lines $ cs txt
  where
    words'' = map cs words'

matchWords :: [Text] -> A.Parser Text
matchWords words' = mconcat <$> traverse (\w -> mappend <$> skipBefore w <*> A.string w) words'

skipBefore :: Text -> A.Parser Text
skipBefore word = pack <$> (A.manyTill A.anyChar . A.lookAhead $ A.string word)
