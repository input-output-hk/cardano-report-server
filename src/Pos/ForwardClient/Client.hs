{-# LANGUAGE OverloadedStrings #-}

module Pos.ForwardClient.Client
    ( getAgentID
    , uploadLogs
    , createTicket
    , defCr
    ) where

import           Universum

import           Control.Lens ((?~))
import           Data.Aeson.Lens (key, _Integer, _String)
import qualified Data.ByteString.Lazy as BSL
import           Network.Wreq (Options, auth, basicAuth, defaults, getWith, header, partFile,
                               postWith, responseBody)

import           Pos.ForwardClient.Types (Agent (..), CrTicket (..), CustomReport (..), Token)
import           Pos.ReportServer.Util (prettifyJson)

-- defAgent :: Agent
-- defAgent = Agent "stavrikios@gmail.com" "V8j8Kv3w5ZJY5huaT1BlwChybe5cZsh3ykVg8uG6" 360221985593

api :: String
api = "https://iohksupport.zendesk.com/api/v2/"

defCr :: CustomReport
defCr = CustomReport "user@email.com" "sample problem" "sample description"

agentOpts :: Agent -> Options
agentOpts (Agent email token _) =
    defaults & auth ?~ basicAuth (encodeUtf8 (email <> "/token")) (encodeUtf8 token)

agentOptsJson :: Agent -> Options
agentOptsJson (Agent email token _) =
    defaults & header "content-type" .~ ["application/json"]
             & auth ?~ basicAuth (encodeUtf8 (email <> "/token")) (encodeUtf8 token)

-- | Queries the zendesk api to get the agent's id
getAgentID :: Agent -> IO Integer
getAgentID agent = do
    r <- getWith (agentOpts agent) (api ++ "locales/current.json")
    let tok =
            fromMaybe (error "getAgentId: couldn't retrieve locale.id field") $
            r ^? responseBody . key "locale" . key "id" . _Integer
    pure tok

-- | Merges the log files, uploads them to Zendesk and returns the token from zendesk.
-- The name of the upload is defaulted to logs.log
uploadLogs :: Agent -> [(FilePath, LByteString)] -> IO Token
uploadLogs agent logs =  do
    BSL.writeFile "logs.log" $ joinLogs logs
    r <- postWith (agentOpts agent)
                  (api ++ "uploads.json?filename=logs.log")
                  (partFile "log" "logs.log")
    let Just tok = r ^? responseBody . key "upload" . key "token" . _String
    pure tok
  where
    joinLogs :: [(FilePath, LByteString)] -> LByteString
    joinLogs = mconcat . map snd

createTicket :: Agent -> CustomReport -> [(FilePath, LByteString)] -> IO LByteString
createTicket agent cr logs = do
    attachToken <- uploadLogs agent logs
    let ticket = CrTicket agent cr attachToken
    putStrLn $ prettifyJson ticket
    r <- postWith (agentOptsJson agent)
                  (api ++ "tickets.json")
                  (encodeUtf8 (prettifyJson ticket) :: ByteString)
    pure $ r ^. responseBody
