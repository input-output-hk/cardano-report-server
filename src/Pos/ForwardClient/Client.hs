{-# LANGUAGE OverloadedStrings #-}

module Pos.ForwardClient.Client
    ( getAgentID
    , createTicket
    , getTicketID
    , ZendeskParams(..)
    ) where

import           Universum

import           Control.Lens ((?~))
import           Data.Aeson.Lens (key, _Integer, _String)
import           Network.Wreq (Options, auth, basicAuth, defaults, getWith, header, postWith,
                               responseBody)

import           Pos.ForwardClient.Types (Agent (..), AgentId (..), CrTicket (..),
                                          CustomReport (..), Token)
import           Pos.ReportServer.Util (prettifyJson)

data ZendeskParams = ZendeskParams
    { zpAgent    :: Agent
    , zpAgentId  :: AgentId
    , zpSendLogs :: Bool
    }

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

getTicketID :: LByteString -> Maybe Integer
getTicketID r = r ^? key "ticket" . key "id" . _Integer

-- | Merges the log files, uploads them to Zendesk and returns the token from zendesk.
-- The name of the upload is defaulted to logs.log
uploadLogs :: Agent -> [(FilePath, LByteString)] -> IO Token
uploadLogs agent [(fileName, content)] = do
    r <- postWith (agentOpts agent)
                  (api ++ "uploads.json?filename=" ++ fileName)
                  content
    let Just tok = r ^? responseBody . key "upload" . key "token" . _String
    pure tok
uploadLogs _ _ = error "Multiple files not allowed."

-- | Creates ticket and uploads logs.
createTicket ::
       CustomReport
    -> [(FilePath, LByteString)]
    -> ZendeskParams
    -> IO LByteString
createTicket cr logs ZendeskParams {..} = do
    attachToken <-
        if zpSendLogs && length logs > 0
        then Just <$> uploadLogs zpAgent logs
        else pure Nothing
    let ticket = CrTicket zpAgentId cr attachToken
    putStrLn $ prettifyJson ticket
    r <- postWith (agentOptsJson zpAgent)
                  (api ++ "tickets.json")
                  (encodeUtf8 (prettifyJson ticket) :: ByteString)
    pure $ r ^. responseBody
