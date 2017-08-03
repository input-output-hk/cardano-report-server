{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.ReportServer.Server
       ( reportServerApp
       , limitBodySize
       ) where

import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Aeson                  (ToJSON, eitherDecode)
import           Data.Aeson.Encode.Pretty    (encodePretty)
import qualified Data.ByteString.Lazy        as BSL
import           Data.List                   ((\\))
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE (decodeUtf8)
import qualified Data.Text.Lazy              as LT
import           Network.HTTP.Types.Status   (status200, status404, status413, Status)
import           Network.Wai                 (Middleware, RequestBodyLength (..),
                                              Application, requestBodyLength, responseLBS,
                                              Response, ResponseReceived, Request, requestHeaders
                                             )
import           Network.Wai.UrlMap (mapUrls, mount, mountRoot)
import           Network.Wai.Parse           (fileContent)
import           Universum

import           Pos.ReportServer.ClientInfo (clientInfo)
import           Pos.ReportServer.Exception  (ReportServerException (BadRequest), tryAll)
import           Pos.ReportServer.FileOps    (LogsHolder, addEntry)
import           Pos.ReportServer.Report     (ReportInfo (..))


limitBodySize :: Word64 -> Middleware
limitBodySize limit application request responseHandler =
    if exceeds
        then onExceeds
        else application request responseHandler
  where
    exceeds =
        case requestBodyLength request of
            ChunkedBody   -> False
            KnownLength l -> l > limit
    onExceeds =
        responseHandler $ responseLBS
            status413
            [("Content-Type", "text/plain")]
            "Request body too large to be processed."

liftAndCatchIO
    :: (MonadIO m, MonadCatch m, MonadBaseControl IO m)
    => IO a -> m (Either ReportServerException a)
liftAndCatchIO = tryAll . liftIO

type Responder = Response -> IO ResponseReceived

withStatus :: Status -> T.Text -> Request -> Response
withStatus status msg req = responseLBS status (requestHeaders req) (encodeUtf8 msg)


reportApp :: LogsHolder -> Application
reportApp holder req respond =
    respond (withStatus status200 "To be done" req)

{-
    post "/report" $ do
        (payload :: ReportInfo) <-
            either failPayload pure . eitherDecode =<< param "payload"
        logFiles <-
            map (bimap LT.toStrict $ TE.decodeUtf8 . BSL.toStrict . fileContent) <$>
            files
        let missingLogs = rLogs payload \\ map fst logFiles
        unless (null missingLogs) $ failMissingLogs missingLogs
        let neededLogs = filter ((`elem` rLogs payload) . fst) logFiles
        let payloadFile = ("payload.json", prettifyJson payload)
        let cInfo = clientInfo req
        let clientInfoFile = ("client.info", prettifyJson cInfo)
        liftAndCatchIO $ addEntry holder $ payloadFile : clientInfoFile : neededLogs
        status status200
-}

notFound :: Application
notFound req respond = respond (withStatus status404 "Not found" req)

reportServerApp :: LogsHolder -> Application
reportServerApp holder = mapUrls $
        mount "report" (reportApp holder)
    <|> mountRoot notFound
  where
    prettifyJson :: (ToJSON a) => a -> Text
    prettifyJson = TE.decodeUtf8 . BSL.toStrict . encodePretty
    failMissingLogs missing =
        throwM $ BadRequest $ "Logs mentioned in payload were not attached: " <> show missing
    failPayload e =
        throwM $ BadRequest $ "Couldn't manage to parse json payload: " <> T.pack e
