{-# LANGUAGE OverloadedStrings #-}

module Pos.ForwardClient.Client
    ( getAgentID
    , createTicket
    , createPostTicket
    , getTicketID
    , ZendeskParams(..)
    , createTicketPure
    ) where

import           Universum

import           Control.Lens ((?~))
import           Data.Aeson.Lens (key, _Integer, _String)
import qualified Data.Text as T
import           Network.Wreq (Options, auth, basicAuth, defaults, getWith, header, postWith,
                               responseBody)

import           Pos.ForwardClient.Types (Agent (..), AgentId (..), CrTicket (..), CustomField (..),
                                          CustomReport (..), Token (..), getToken)
import           Pos.ReportServer.Report (IOHKProduct (..), Network (..), ProductVersion (..),
                                          showNetwork)

import           Pos.ReportServer.Util (prettifyJson)

data ZendeskParams = ZendeskParams
    { zpAgent    :: Agent
    , zpAgentId  :: AgentId
    , zpSendLogs :: Bool
    }

api :: Agent -> String
api agent = "https://" ++ T.unpack (aAccount agent) ++ ".zendesk.com/api/v2/"

agentOpts :: Agent -> Options
agentOpts (Agent email token _) =
    defaults & auth ?~ basicAuth (encodeUtf8 (email <> "/token")) (encodeUtf8 (getToken token))

agentOptsJson :: Agent -> Options
agentOptsJson (Agent email token _) =
    defaults & header "content-type" .~ ["application/json"]
             & auth ?~ basicAuth (encodeUtf8 (email <> "/token")) (encodeUtf8 (getToken token))

-- | Queries the zendesk api to get the agent's id
-- | See https://developer.zendesk.com/rest_api/docs/core/users#show-the-currently-authenticated-user
getAgentID :: Agent -> IO AgentId
getAgentID agent = do
   let userUri = api agent ++ "users/me.json"
   r <- getWith (agentOpts agent) userUri
   --putStrLn $ r ^. responseBody
   let tok = fromMaybe
                 ( error $ T.pack
                     ( "getAgentId: Couldn't retrieve `id` field from `user`. "
                       ++ "Check if the user does really exist in account `"
                       ++ T.unpack (aAccount agent)
                       ++ "`: "
                       ++ userUri
                     )
                 )
                 $ r ^? responseBody . key "user" . key "id" . _Integer
   pure $ AgentId tok

getTicketID :: LByteString -> Maybe Integer
getTicketID r = r ^? key "ticket" . key "id" . _Integer

-- | Merges the log files, uploads them to Zendesk and returns the token from zendesk.
-- The name of the upload is defaulted to logs.log
uploadLogs :: Agent -> [(FilePath, LByteString)] -> IO Text
uploadLogs agent [(fileName, content)] = do
    r <- postWith (agentOpts agent)
                  (api agent ++ "uploads.json?filename=" ++ fileName)
                  content
    let Just tok = r ^? responseBody . key "upload" . key "token" . _String
    pure tok
uploadLogs _ _ = error "Multiple files not allowed."


-- | Creates ticket and uploads logs.
createTicket
    :: CustomReport
    -> [(FilePath, LByteString)]
    -> ZendeskParams
    -> IO LByteString
createTicket cr logs ZendeskParams {..} = do
    attachToken <-
        if zpSendLogs && length logs > 0
        then Just <$> uploadLogs zpAgent logs
        else pure Nothing
    let ticket = CrTicket zpAgentId cr attachToken []
    putStrLn $ prettifyJson ticket
    r <- postWith (agentOptsJson zpAgent)
                  (api zpAgent ++ "tickets.json")
                  (encodeUtf8 (prettifyJson ticket) :: ByteString)
    pure $ r ^. responseBody

-- | Creates ticket and uploads logs, new version.
createPostTicket
    :: CustomReport
    -> [(FilePath, LByteString)]
    -> ZendeskParams
    -> IOHKProduct
    -> ProductVersion
    -> Network
    -> IO LByteString
createPostTicket cr logs ZendeskParams{..} iohkProduct productVersion network = do
    attachToken <-
        if zpSendLogs && length logs > 0
            then Just <$> uploadLogs zpAgent logs
            else pure Nothing
    -- TODO(ks): Left to implement!
    let customFields =
            [ CustomField 114100735553 validProductNetwork
            , CustomField 360007053414 validProductVersion
            ]
    let ticket = CrTicket zpAgentId cr attachToken customFields

    putTextLn "Sent to Zendesk:"
    putStrLn $ prettifyJson ticket

    r <- postWith (agentOptsJson zpAgent)
                  (api zpAgent ++ "tickets.json")
                  (encodeUtf8 (prettifyJson ticket) :: ByteString)

    putTextLn "Got response from Zendesk:"
    putStrLn $ r ^. responseBody

    pure $ r ^. responseBody
  where
    validProductNetwork = validProduct <> "-" <> showNetwork network
    validProduct        = toValidTag $ getIOHKProduct iohkProduct

    validProductVersion = toValidTag $ getProductVersion productVersion

    toValidTag :: Text -> Text
    toValidTag = T.intercalate "_" . words . T.toLower

-- For trying it out. This is what we send to Zendesk.
-- putStrLn createTicketPure
createTicketPure :: Text
createTicketPure = prettifyJson $ CrTicket agentId customReport Nothing customFields
  where

    customReport :: CustomReport
    customReport = CustomReport "cardano-report-server-test@iohk.io" "This is a test for cardano report server" "Testing"

    _logs :: [(FilePath, LByteString)]
    _logs = []

    agentId :: AgentId
    agentId = AgentId 360607889193

    token :: Token
    token = Token . fromString $ replicate 20 '0'

    agent :: Agent
    agent = Agent "kristijan.saric@iohk.io" token "account"

    _zendeskParams :: ZendeskParams
    _zendeskParams = ZendeskParams agent agentId False

    customProductField :: CustomField
    customProductField = CustomField 114100735553 "daedalus_wallet-mainet"

    customProductVersionField :: CustomField
    customProductVersionField = CustomField 360007053414 "daedalus_0.11.0_cardano_1.3.0"

    customFields :: [CustomField]
    customFields = [customProductField, customProductVersionField]


