{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Monad.Trans.Control          (MonadBaseControl)
import           Data.Aeson                           (eitherDecode)
import           Data.Aeson.Encode.Pretty             (encodePretty)
import qualified Data.ByteString.Lazy                 as BSL
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as TE (decodeUtf8)
import qualified Data.Text.Lazy                       as LT
import           Network.HTTP.Types.Status            (status200, status404)
import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Parse                    (fileContent)
import           Universum
import           Web.Scotty.Trans                     (files, notFound, param, post,
                                                       raise, status, text)
import qualified Web.Scotty.Trans                     as S

import           Exception                            (ReportServerException (BadRequest),
                                                       tryAll)
import           FileOps                              (LogsHolder, addEntry, initHolder)
import           Options                              (Opts (..), getOptions)
import           Report                               (ReportInfo (..))


main :: IO ()
main = do
    a@Opts{..} <- getOptions
    putText $ "Started with options: " <> show a
    holder <- initHolder logsDir
    putText "Successfully created holder"

    putText "Launching server..."
    application <- S.scottyAppT liftIO $ app holder
    Warp.run port $ logStdoutDev application

liftAndCatchIO
    :: (MonadIO m, MonadCatch m, MonadBaseControl IO m)
    => IO a -> S.ActionT (ReportServerException) m a
liftAndCatchIO action = either raise pure =<< tryAll (liftIO action)

app :: LogsHolder -> S.ScottyT ReportServerException IO ()
app holder = do
    post "/report" $ do
        (payload :: ReportInfo) <-
            either failPayload pure . eitherDecode =<< param "payload"
        logFiles <-
            map (bimap LT.toStrict $ TE.decodeUtf8 . BSL.toStrict . fileContent) <$>
            files
        let payloadFile =
                ( "payload.json"
                , TE.decodeUtf8 $ BSL.toStrict (encodePretty payload))
        liftAndCatchIO $ addEntry holder $ payloadFile : logFiles
        status status200
    notFound err404
  where
    err404 = status status404 >> text "Not found"
    failPayload e =
        raise $ BadRequest $ "Couldn't manage to parse json payload: " <> T.pack e
