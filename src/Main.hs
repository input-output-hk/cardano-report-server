module Main (main) where

import qualified Data.ByteString.Lazy                 as BSL
import qualified Data.Text.Encoding                   as TE (decodeUtf8)
import qualified Data.Text.Lazy                       as LT
import           Network.HTTP.Types.Status            (status200, status404)
import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Parse                    (fileContent)
import           System.Directory                     (createDirectoryIfMissing,
                                                       renameFile)
import           System.FilePath                      (takeFileName, (</>))
import           Universum
import           Web.Scotty                           as S

import           FileOps                              (addEntry, initHolder)
import           Options                              (Opts (..), getOptions)

main :: IO ()
main = do
    a@Opts{..} <- getOptions
    putText $ "Started with options: " <> show a
    holderVar <- newMVar =<< initHolder logsDir
    putText "Successfully created holder"

    app <- scottyApp $ do
        let err404 = status status404 >> text "Not found"
        post "/report" $ do
            payload <- ("description.json",) <$> param "payload"
            logFiles <-
                map (bimap LT.toStrict $ TE.decodeUtf8 . BSL.toStrict . fileContent) <$>
                files
            addEntry holderVar $ payload:logFiles
            status status200
        notFound err404

    putText "Launching server..."
    Warp.run port $ logStdoutDev app
