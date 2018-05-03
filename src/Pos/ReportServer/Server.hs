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
import           Network.Wai.UrlMap (mapUrls, mount, mountRoot)
import           System.IO (hPutStrLn)

import           Pos.ForwardClient.Client (ZendeskParams (..), createTicket)
import           Pos.ForwardClient.Types (CustomReport (..))
import           Pos.ReportServer.ClientInfo (clientInfo)
import           Pos.ReportServer.Exception (ReportServerException (BadRequest, ParameterNotFound),
                                             tryAll)
import           Pos.ReportServer.FileOps (LogsHolder, addEntry, storeCustomReport)
import           Pos.ReportServer.Report (ReportInfo (..), ReportType (..))
import           Pos.ReportServer.Util (prettifyJson)

import           Pos.Analyser.Analysis (grabKnowledgebase, analyse)

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
withStatus status msg req = responseLBS status (requestHeaders req) (encodeUtf8 msg)

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

analyseApp :: ServerContext -> Application
analyseApp ServerContext{..} req respond = do
    case parseMethod (requestMethod req) of
        Right POST -> do
        -- ^ Server only accepts POST requests
            (!params, !files) <- bodyParse req
            let failPayload e = throwM $ BadRequest $ "Couldn't manage to parse json payload: " <> T.pack e
            !(payload :: ReportInfo) <- either failPayload pure . eitherDecodeStrict =<< param "payload" params
            -- ^ Server parsed payload
            let logFiles = map (bimap decodeUtf8 fileContent) files
            let clientInfoFile = ("client.info", encodeUtf8 $ prettifyJson (clientInfo req))
            let logsAndClientInfo = clientInfoFile : logFiles
            internalResponse <- liftAndCatchIO $ do
            -- ^ All internal computations of data in this block
              case rReportType payload of
                RAnalyse -> do
                    case logFiles of
                        [(_,contents)] -> do
                            base <- grabKnowledgebase "knowledgebase.csv"
                            addEntry scLogsHolder payload logsAndClientInfo
                            pure $ analyse contents base
                        _ -> throwIO $ BadRequest "Multiple files not allowed for analysis."
                _ -> throwIO $ BadRequest "Instruction not supported."
            -- ^ End of computations
            case internalResponse of
                Right issues -> let serverResponse = T.intercalate "\n"
                                                      [ T.intercalate ", " (decodeUtf8 <$> record) | record <- issues]
                                in  do
                                  putStrLn $ "Analysis Results:\n" <> serverResponse
                                  respond (with200Response serverResponse req)
                    -- ^ Internal computations succeded
                Left e -> do
                    hPutStrLn stderr ("An exception occured: " <> displayException e)
                    respond (with500Response req)
                    -- ^ Internal computations failed
        _  -> respond (with404Response req)

reportApp :: ServerContext -> Application
reportApp ServerContext{..} req respond = do
    case parseMethod (requestMethod req) of
        Right POST -> do
        -- ^ Server only accepts POST requests
            (!params, !files) <- bodyParse req
            let failPayload e = throwM $ BadRequest $ "Couldn't manage to parse json payload: " <> T.pack e
            !(payload :: ReportInfo) <- either failPayload pure . eitherDecodeStrict =<< param "payload" params
            -- ^ Server parsed payload
            let logFiles = map (bimap decodeUtf8 fileContent) files
            let clientInfoFile = ("client.info", encodeUtf8 $ prettifyJson (clientInfo req))
            let logsAndClientInfo = clientInfoFile : logFiles
            -- ^ Server appends clientInfoFile to logFiles
            internalResponse <- liftAndCatchIO $ do
            -- ^ All internal computations of data in this block
                case rReportType payload of
                    cr@RCustomReport{} -> do
                        when (length logFiles > 1) $
                            throwIO $ BadRequest "Multiple files not allowed in custom reports."
                        case scZendeskParams of
                            Nothing -> throwIO $ BadRequest "Zendesk parameters missing."
                            Just parameters -> do
                                let RCustomReport{..} = cr
                                let customReport = CustomReport crEmail crSubject crProblem
                                zendeskResponse <- createTicket customReport logFiles parameters
                                -- ^ Server has a response from zendesk
                                when scStoreCustomReports $
                                    storeCustomReport scLogsHolder
                                                        payload
                                                        logsAndClientInfo
                                                        zendeskResponse
                                -- ^ Server stores the custom report and zendesk's response if necessary
                                pure $ Just zendeskResponse
                    _ -> addEntry scLogsHolder payload logsAndClientInfo >> pure Nothing
                    -- ^ Server stores every report other than [RAnalyze, RCustomReports]
            case internalResponse of
                Right response ->
                    let serverResponse = fromMaybe "Success" $ decodeUtf8 <$> response
                    in  respond (with200Response serverResponse req)
                    -- ^ Internal computations succeded
                Left e -> do
                    hPutStrLn stderr ("An exception occured: " <> displayException e)
                    respond (with500Response req)
                    -- ^ Internal computations failed
        _  -> respond (with404Response req)

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
        mount "report" (reportApp context) <|>
        mount "analyse" (analyseApp context) <|>
        mountRoot notFound