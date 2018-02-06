{-# LANGUAGE OverloadedStrings #-}

module Pos.ForwardClient.Client
       ( getAgentID
       , createTicket
       ) where

import           Universum

import           Control.Lens ((?~))
import           Data.Aeson.Lens (key, _Integer, _String)
import qualified Data.ByteString.Lazy as BSL
import           Network.Wreq (Options, auth, basicAuth, defaults, getWith, header, partFile,
                               postWith, responseBody)

import           Pos.ForwardClient.Types (Agent (..), AgentId (..), CrTicket (..),
                                          CustomReport (..), Token)
import           Pos.ReportServer.Util (prettifyJson)

api :: String
api = "https://iohksupport.zendesk.com/api/v2/"

agentOpts :: Agent -> Options
agentOpts (Agent email token) =
    defaults & auth ?~ basicAuth (encodeUtf8 (email <> "/token")) (encodeUtf8 token)

agentOptsJson :: Agent -> Options
agentOptsJson (Agent email token) =
    defaults & header "content-type" .~ ["application/json"]
             & auth ?~ basicAuth (encodeUtf8 (email <> "/token")) (encodeUtf8 token)

-- | Queries the zendesk api to get the agent's id
getAgentID :: Agent -> IO AgentId
getAgentID agent = do
    r <- getWith (agentOpts agent) (api ++ "users/me.json")
    let tok =
            fromMaybe (error "getAgentId: couldn't retrieve locale.id field") $
            r ^? responseBody . key "user" . key "id" . _Integer
    pure $ AgentId tok

-- | Merges the log files, uploads them to Zendesk and returns the token from zendesk.
-- The name of the upload is defaulted to logs.log
uploadLogs :: Agent -> [(FilePath, LByteString)] -> IO Token
uploadLogs agent logs = do
    BSL.writeFile "logs.log" $ joinLogs logs
    r <- postWith (agentOpts agent)
                  (api ++ "uploads.json?filename=logs.log")
                  (partFile "log" "logs.log")
    let Just tok = r ^? responseBody . key "upload" . key "token" . _String
    pure tok
  where
    joinLogs :: [(FilePath, LByteString)] -> LByteString
    joinLogs = mconcat . map snd

-- | Creates ticket and uploads logs.
createTicket :: Agent -> AgentId -> CustomReport -> [(FilePath, LByteString)] -> IO LByteString
createTicket agent agentId cr logs = do
    attachToken <- uploadLogs agent logs
    let ticket = CrTicket agentId cr attachToken
    putStrLn $ prettifyJson ticket
    r <- postWith (agentOptsJson agent)
                  (api ++ "tickets.json")
                  (encodeUtf8 (prettifyJson ticket) :: ByteString)
    pure $ r ^. responseBody