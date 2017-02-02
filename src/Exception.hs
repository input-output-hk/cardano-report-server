{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Local exception hierarchy.

module Exception
    ( ReportServerException(..)
    , tryAll
    ) where

import qualified Control.Exception.Lifted    as LiftedBase
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Universum
import           Web.Scotty.Internal.Types   as S
import           Web.Scotty.Trans            as T

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

instance ScottyError ReportServerException where
    showError = show
    stringError t = FromTextException t

instance (MonadThrow m, ScottyError e) => MonadThrow (T.ActionT e m) where
    throwM = lift . throwM

instance (MonadCatch m, ScottyError e, MonadBaseControl IO m) =>
         MonadCatch (T.ActionT e m) where
    catch = LiftedBase.catch
