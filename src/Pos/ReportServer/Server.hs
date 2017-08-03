{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Pos.ReportServer.Server
       ( reportServerApp
       , limitBodySize
       ) where

import           Control.Exception           (displayException)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Aeson                  (ToJSON, eitherDecodeStrict)
import           Data.Aeson.Encode.Pretty    (encodePretty)
import qualified Data.ByteString.Lazy        as BSL
import           Data.List                   (lookup, (\\))
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE (decodeUtf8)
import           Network.HTTP.Types          (StdMethod (POST), parseMethod)
import           Network.HTTP.Types.Status   (Status, status200, status404,
                                              status413, status500)
import           Network.Wai                 (Application, Middleware, Request,
                                              RequestBodyLength (..), Response,
                                              requestBodyLength, requestHeaders,
                                              requestMethod, responseLBS)
import           Network.Wai.Parse           (File, Param,
                                              defaultParseRequestBodyOptions,
                                              fileContent, lbsBackEnd,
                                              parseRequestBodyEx)
import           Network.Wai.UrlMap          (mapUrls, mount, mountRoot)
import           Universum

import           Pos.ReportServer.ClientInfo (clientInfo)
import           Pos.ReportServer.Exception  (ReportServerException (BadRequest),
                                              tryAll)
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

withStatus :: Status -> T.Text -> Request -> Response
withStatus status msg req = responseLBS status (requestHeaders req) (encodeUtf8 msg)

-- | Gets the list of the uploaded files.
bodyParse :: Request -> IO ([Param], [File LByteString])
bodyParse request = do
    let parseBodyOptions = defaultParseRequestBodyOptions
    parseRequestBodyEx parseBodyOptions lbsBackEnd request

-- Tries to retrieve the `ReportInfo` from the raw `Request`.
param :: ByteString -> [Param] -> ByteString
param key = fromMaybe mempty . lookup key

reportApp :: LogsHolder -> Application
reportApp holder req respond =
    case parseMethod (requestMethod req) of
        Right POST -> do
          (params, files) <- bodyParse req
          (payload :: ReportInfo) <-
              either failPayload pure . eitherDecodeStrict =<< return (param "payload" params)
          let logFiles = map (bimap decodeUtf8 $ TE.decodeUtf8 . BSL.toStrict . fileContent) files
          let missingLogs = rLogs payload \\ map fst logFiles
          unless (null missingLogs) $ failMissingLogs missingLogs
          let neededLogs = filter ((`elem` rLogs payload) . fst) logFiles
          let payloadFile = ("payload.json", prettifyJson payload)
          let cInfo = clientInfo req
          let clientInfoFile = ("client.info", prettifyJson cInfo)
          res <- liftAndCatchIO $ addEntry holder $ payloadFile : clientInfoFile : neededLogs
          case res of
              Left e -> respond (with500Response (toText $ displayException e) req)
              _      -> respond (with200Response req)
        _  -> respond (with404Response req)
  where
    prettifyJson :: (ToJSON a) => a -> Text
    prettifyJson = TE.decodeUtf8 . BSL.toStrict . encodePretty
    failMissingLogs missing =
        throwM $ BadRequest $ "Logs mentioned in payload were not attached: " <> show missing
    failPayload e =
        throwM $ BadRequest $ "Couldn't manage to parse json payload: " <> T.pack e

with404Response :: Request -> Response
with404Response = withStatus status404 "Not found"

with200Response :: Request -> Response
with200Response = withStatus status200 mempty

with500Response :: Text -> Request -> Response
with500Response = withStatus status500

notFound :: Application
notFound req respond = respond (with404Response req)

reportServerApp :: LogsHolder -> Application
reportServerApp holder = mapUrls $
        mount "report" (reportApp holder)
    <|> mountRoot notFound
