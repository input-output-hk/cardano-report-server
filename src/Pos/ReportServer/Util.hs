-- | Utilities.

module Pos.ReportServer.Util
       ( withFileReadLifted
       , withFileWriteLifted
       , prettifyJson
       ) where

import           Universum

import           Data.Aeson               (ToJSON)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as BSL
import qualified Data.Text.Encoding       as TE (decodeUtf8)
import           System.FileLock          (SharedExclusive (..), lockFile, unlockFile)


-- | Execute MonadIO action with read/shared lock on the file.
withFileReadLifted :: (MonadIO m, MonadMask m) => FilePath -> m a -> m a
withFileReadLifted = withFileModLifted Shared

-- | Execute MonadIO action with write/exclusive lock on the file.
withFileWriteLifted :: (MonadIO m, MonadMask m) => FilePath -> m a -> m a
withFileWriteLifted = withFileModLifted Exclusive

withFileModLifted
    :: (MonadIO m, MonadMask m)
    => SharedExclusive -> FilePath -> m a -> m a
withFileModLifted lockType fp action =
    bracket (liftIO $ lockFile fp lockType)
            (\x -> liftIO $ unlockFile x)
            (const action)

-- | Encodes json file in a pretty way.
prettifyJson :: (ToJSON a) => a -> Text
prettifyJson = TE.decodeUtf8 . BSL.toStrict . encodePretty
