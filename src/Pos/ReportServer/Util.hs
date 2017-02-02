-- | Utilities

module Pos.ReportServer.Util
       ( withFileReadLifted
       , withFileWriteLifted
       ) where

import           System.FileLock (SharedExclusive (..), lockFile, unlockFile)
import           Universum

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
