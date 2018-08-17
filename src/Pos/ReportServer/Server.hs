{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Pos.ReportServer.Server
       ( ServerContext(..)
       , reportServerApp
       , limitBodySize
       ) where

import           Universum

import           Control.Exception (displayException, throwIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Aeson (eitherDecodeStrict)
import           Data.List (lookup)
import qualified Data.Text as T
import           Network.HTTP.Types (StdMethod (POST), parseMethod)
import           Network.HTTP.Types.Status (Status, status200, status404, status413, status500)
import           Network.Wai (Application, Middleware, Request, RequestBodyLength (..), Response,
                              requestBodyLength, requestHeaders, requestMethod, responseLBS)
import           Network.Wai.Parse (File, Param, defaultParseRequestBodyOptions, fileContent,
                                    lbsBackEnd, parseRequestBodyEx)
import           Network.Wai.UrlMap (mapUrls, mount, mount', mountRoot)
import           System.IO (hPutStrLn)

import           Pos.ForwardClient.Client (ZendeskParams (..), createPostTicket, createTicket)
import           Pos.ForwardClient.Types (CustomReport (..))
import           Pos.ReportServer.ClientInfo (clientInfo)
import           Pos.ReportServer.Exception (ReportServerException (BadRequest, ParameterNotFound),
                                             tryAll)
import           Pos.ReportServer.FileOps (LogsHolder, addEntry, storeCustomReport)
import           Pos.ReportServer.Report (ReportInfo (..), ReportType (..), V1ReportInfo (..),
                                          generateProductVersion)
import           Pos.ReportServer.Util (prettifyJson)


data ServerContext = ServerContext
    { scZendeskParams      :: !(Maybe ZendeskParams) -- ^ If Nothing, zd is turned off
    , scStoreCustomReports :: !Bool -- ^ If we store logs on custom
                                   -- report. This will only be used
                                   -- if zendesk integration is on,
                                   -- because we store response too.
    , scLogsHolder         :: !LogsHolder
    }

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
withStatus status msg req = responseLBS status mempty (encodeUtf8 msg)

-- | Gets the list of the uploaded files.
bodyParse :: Request -> IO ([Param], [File LByteString])
bodyParse request = do
    let parseBodyOptions = defaultParseRequestBodyOptions
    parseRequestBodyEx parseBodyOptions lbsBackEnd request

-- Tries to retrieve the `ReportInfo` from the raw `Request`, throwing an exception if
-- the parameter cannot be found.
param :: ByteString -> [Param] -> IO ByteString
param key ps = case lookup key ps of
    Just val -> return val
    Nothing  -> throwIO $ ParameterNotFound (decodeUtf8 key)

reportApp :: ServerContext -> Application
reportApp ServerContext{..} req respond =
    case parseMethod (requestMethod req) of
        Right POST -> do

          (!params, !files) <- bodyParse req
          !(payload :: ReportInfo) <-
              either failPayload pure . eitherDecodeStrict =<< param "payload" params

          let logFiles = map (bimap decodeUtf8 fileContent) files
          let cInfo = clientInfo req
          let clientInfoFile = ("client.info", encodeUtf8 $ prettifyJson cInfo)

          res <- liftAndCatchIO $ do
              let allLogs = clientInfoFile : logFiles

              -- Send data to zendesk if needed.
              zResp <- case (scZendeskParams, rReportType payload) of

                (Just zp, RCustomReport{..}) -> do

                    let cr = CustomReport crEmail crSubject crProblem

                    when (length logFiles > 1) $
                       throwIO $ BadRequest "Multiple files not allowed with custom reports."

                    response <- createTicket cr logFiles zp

                    when (scStoreCustomReports) $
                        storeCustomReport scLogsHolder payload allLogs response

                    pure $ Just response

                (Nothing, r@RCustomReport{}) -> do
                    let e = "Ignoring custom report because zendesk " <>
                              "is turned off: " <> show r
                    putStrLn e
                    pure $ Just e

                _  -> pure Nothing

              -- store report locally if it's not custom
              case rReportType payload of
                  RCustomReport{} -> pass
                  _               -> addEntry scLogsHolder payload allLogs

              pure zResp

          case res of
              Right maybeZDResp -> do
                  let respText = fromMaybe "Success" $ decodeUtf8 <$> maybeZDResp
                  respond (with200Response respText req)
              Left e -> do
                  let ex = displayException e
                  hPutStrLn stderr ("An exception occurred: " <> ex)
                  respond (with500Response req)

        _  -> respond (with404Response req)
  where
    failPayload e =
        throwM $ BadRequest $ "Couldn't manage to parse json payload: " <> T.pack e


-- | The new version report server. For now it's a stupid copy-and-paste, requires
-- substantial refactoring to remove duplication.
reportV1App :: ServerContext -> Application
reportV1App ServerContext{..} req respond =
    case parseMethod (requestMethod req) of
        Right POST -> do

          (!params, !files) <- bodyParse req
          payload <- either failPayload pure . eitherDecodeStrict =<< param "payload" params

          let logFiles = map (bimap decodeUtf8 fileContent) files

          res <- liftAndCatchIO $ handleV1ReportEndpoint payload logFiles scZendeskParams

          case res of
              Right maybeZDResp -> do
                  let respText = fromMaybe "Success" $ decodeUtf8 <$> maybeZDResp
                  respond (with200Response respText req)
              Left e -> do
                  let ex = displayException e
                  hPutStrLn stderr ("An exception occurred: " <> ex)
                  respond (with500Response req)

        _  -> respond (with404Response req)
  where
    failPayload e =
        throwM $ BadRequest $ "Couldn't manage to parse json payload: " <> T.pack e


-- | The handle for the new report endpoint.
handleV1ReportEndpoint
    :: V1ReportInfo
    -> [(FilePath, LByteString)]
    -> Maybe ZendeskParams
    -> IO (Maybe LByteString)
handleV1ReportEndpoint payload logFiles scZendeskParams = do

    -- Send data to zendesk if needed.
    zResp <- case (scZendeskParams, riReportType payload) of

      (Just zendeskParams, RCustomReport{..}) -> do
          -- For the product and product
          let productInfo       = riProduct payload
          let productVersion    = generateProductVersion payload
          let network           = riNetwork payload

          -- No point in this.
          let cr              = CustomReport crEmail crSubject crProblem

          when (length logFiles > 1) $
             throwIO $ BadRequest "Multiple files not allowed with custom reports."

          -- Here we create a ticket and post it to Zendesk.
          response <- createPostTicket cr logFiles zendeskParams productInfo productVersion network

          pure $ Just response

      (Nothing, r@RCustomReport{}) -> do
          let e = "Ignoring custom report because zendesk " <>
                    "is turned off: " <> show r
          putStrLn e
          pure $ Just e

      _  -> pure Nothing

    pure zResp

with404Response :: Request -> Response
with404Response = withStatus status404 "Not found"

with200Response :: Text -> Request -> Response
with200Response = withStatus status200

with500Response :: Request -> Response
with500Response = withStatus status500 "500 Internal Server Error"

notFound :: Application
notFound req respond = respond (with404Response req)

reportServerApp :: ServerContext -> Application
reportServerApp context =
    mapUrls $
        mount "report"                  v0App
    <|> mount' ["api", "v1", "report"]  v1App
    <|> mountRoot notFound
  where
    v0App = reportApp context
    v1App = reportV1App context
