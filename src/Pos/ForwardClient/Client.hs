{-# LANGUAGE OverloadedStrings #-}

module Pos.ForwardClient.Client where

import Prelude

import Network.Wreq
import Control.Lens
import Control.Monad
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy  as BSL

import Data.Aeson.Lens
import Data.Aeson (encode)

import qualified Data.Text as T
import Data.Text.Encoding

import Pos.ForwardClient.Types
import Pos.ReportServer.Util

opts :: Options
opts = defaults & auth ?~ basicAuth "stavrikios@gmail.com/token" "V8j8Kv3w5ZJY5huaT1BlwChybe5cZsh3ykVg8uG6"

defAgent :: Agent
defAgent = Agent "stavrikios@gmail.com" "V8j8Kv3w5ZJY5huaT1BlwChybe5cZsh3ykVg8uG6" 360221985593

defCr :: CustomReport
defCr = CustomReport "user@email.com" "sample problem" "sample description"

agentOpts :: Agent -> Options
agentOpts (Agent email token _) = defaults & auth ?~ basicAuth (encodeUtf8 (T.append email "/token")) (encodeUtf8 token)

agentOptsJson :: Agent -> Options
agentOptsJson (Agent email token _) = defaults & header "content-type" .~ ["application/json"] & auth ?~ basicAuth (encodeUtf8 (T.append email "/token")) (encodeUtf8 token)

api = "https://iohksupport.zendesk.com/api/v2/"

test :: IO ()
test = do
    r <- getWith opts $ api ++ "users.json"
    print r

testUpload :: IO ()
testUpload = do
    r <- postWith opts (api ++ "uploads.json?filename=test.log") (partFile "log" "test.log")
    print $ r ^. responseBody

-- | Queries the zendesk api to get the agent's id
getAgentID :: Agent -> IO Integer
getAgentID agent = do
    r <- getWith (agentOpts agent) (api ++ "locales/current.json")
    let Just tok = r ^? responseBody . key "locale" . key "id" . _Integer
    return tok

joinLogs :: [(FilePath, BSL.ByteString)] -> BSL.ByteString
joinLogs logs = BSL.concat $ snd <$> logs

-- | Merges the log files, uploads them to Zendesk and returns the token from zendesk.
-- The name of the upload is defaulted to logs.log
uploadLogs :: Agent -> [(FilePath, BSL.ByteString)] -> IO Token
uploadLogs agent logs =  do
    BSL.writeFile "logs.log" $ joinLogs logs
    r <- postWith (agentOpts agent) (api ++ "uploads.json?filename=logs.log") (partFile "log" "logs.log")
    let Just tok = r ^? responseBody . key "upload" . key "token" . _String
    return tok

createTicket :: Agent -> CustomReport -> [(FilePath, BSL.ByteString)] -> IO ()
createTicket agent cr logs = do
    attachToken <- uploadLogs agent logs
    let ticket = CrTicket agent cr attachToken
    putStrLn $ C.unpack $ encodeUtf8 $ prettifyJson ticket
    r <- postWith (agentOptsJson agent) (api ++ "tickets.json") (encodeUtf8 (prettifyJson ticket))
    print $ r ^. responseBody