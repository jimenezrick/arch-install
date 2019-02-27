{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Error where

import RIO

import Data.String.Conversions

throwNothing :: (MonadIO m, HasCallStack) => String -> m (Maybe a) -> m a
throwNothing err x =
    x >>= \case
        Nothing -> throwString err
        (Just a) -> return a

throwLeft :: (ConvertibleStrings a String, MonadIO m, HasCallStack) => m (Either a b) -> m b
throwLeft x =
    x >>= \case
        (Left a) -> throwString $ cs a
        (Right b) -> return b
