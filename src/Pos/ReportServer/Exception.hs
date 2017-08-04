{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Local exception hierarchy.

module Pos.ReportServer.Exception
    ( ReportServerException(..)
    , tryAll
    ) where

import           Universum

data ReportServerException
    = BadRequest Text
      -- ^ Server received bad type of request
    | MalformedIndex Text
      -- ^ Internal storage is malformed
    | FromTextException [Char]
      -- ^ Port of scotty String exceptions
    | ExternalException SomeException
      -- ^ All other exceptions
    deriving (Show)
instance Exception ReportServerException

-- | Converts any error to 'ReportServerException', so scotty is cool
-- with its 'MonadExcept'.
tryAll :: (MonadCatch m) => m a -> m (Either ReportServerException a)
tryAll action = ((Right <$> action) `catch` handler1) `catch` handler2
  where
    handler1 (e :: ReportServerException) = pure $ Left e
    handler2 (e :: SomeException) = pure $ Left (ExternalException e)
