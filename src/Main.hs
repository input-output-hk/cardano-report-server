module Main (main) where

import qualified Data.ByteString.Lazy      as BSL
import           Network.HTTP.Types.Status (status404)
import           System.Directory          (createDirectoryIfMissing, renameFile)
import           System.FilePath           (takeFileName, (</>))
import           Universum
import           Web.Scotty                as S

import           FileOps                   (initHolder)
import           Options                   (Opts (..), getOptions)

main :: IO ()
main = do
    a@Opts{..} <- getOptions
    putText $ "Started with options: " <> show a
    holderVar <- newMVar =<< initHolder logsDir
    putText "Successfully created holder"
