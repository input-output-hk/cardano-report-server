{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.ReportServer.Server
       ( reportServerApp
       ) where

import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Aeson                  (eitherDecode)
import           Data.Aeson.Encode.Pretty    (encodePretty)
import qualified Data.ByteString.Lazy        as BSL
import           Data.List                   ((\\))
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE (decodeUtf8)
import qualified Data.Text.Lazy              as LT
import           Network.HTTP.Types.Status   (status200, status404)
import           Network.Wai.Parse           (fileContent)
import           Universum
import           Web.Scotty.Trans            (files, notFound, param, post, raise, status,
                                              text)
import qualified Web.Scotty.Trans            as S

import           Pos.ReportServer.Exception  (ReportServerException (BadRequest), tryAll)
import           Pos.ReportServer.FileOps    (LogsHolder, addEntry)
import           Pos.ReportServer.Report     (ReportInfo (..))


liftAndCatchIO
    :: (MonadIO m, MonadCatch m, MonadBaseControl IO m)
    => IO a -> S.ActionT (ReportServerException) m a
liftAndCatchIO action = either raise pure =<< tryAll (liftIO action)

reportServerApp :: LogsHolder -> S.ScottyT ReportServerException IO ()
reportServerApp holder = do
    post "/report" $ do
        (payload :: ReportInfo) <-
            either failPayload pure . eitherDecode =<< param "payload"
        logFiles <-
            map (bimap LT.toStrict $ TE.decodeUtf8 . BSL.toStrict . fileContent) <$>
            files
        let missingLogs = rLogs payload \\ map fst logFiles
        unless (null missingLogs) $ failMissingLogs missingLogs
        let neededLogs = filter ((`elem` rLogs payload) . fst) logFiles
        let payloadFile =
                ( "payload.json"
                , TE.decodeUtf8 $ BSL.toStrict (encodePretty payload))
        liftAndCatchIO $ addEntry holder $ payloadFile : neededLogs
        status status200
    notFound err404
  where
    err404 = status status404 >> text "Not found"
    failMissingLogs missing =
        raise $ BadRequest $ "Logs mentioned in payload were not attached: " <> show missing
    failPayload e =
        raise $ BadRequest $ "Couldn't manage to parse json payload: " <> T.pack e
