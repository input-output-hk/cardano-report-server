{-# LANGUAGE OverloadedStrings #-}

-- | Types for sending data to zendesk.
module Pos.ForwardClient.Types
    ( CustomReport(..)
    , Agent(..)
    , AgentOptions(..)
    , Logs
    , Token
    , mkAgent
    , getToken
    , AgentId(..)
    , CrTicket(..)
    ) where

import           Universum

import           Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.Vector as V

data CustomReport = CustomReport
    { crEmail       :: !Text
    , crSubject     :: !Text
    , crDescription :: !Text
    } deriving (Show)

data AgentOptions = AgentOptions
    { aoEmail   :: !Text
    , aoApiToken :: !FilePath
    , aoAccount :: !Text
    } deriving (Show)

data Agent = Agent
        { aEmail   :: !Text
        , apiToken :: !Token
        , aAccount :: !Text
        } deriving (Show)

type Logs = [(FilePath, LByteString)]

newtype Token = Token Text deriving Show

-- | Smart constructor for Token
mkAgent :: AgentOptions -> IO Agent
mkAgent ao = do
  token <- Token <$> readFile (aoApiToken ao)
  return $ Agent
    { aEmail = (aoEmail ao)
    , apiToken = token
    , aAccount = (aoAccount ao)
    }


getToken :: Token -> Text
getToken (Token token) = token

newtype AgentId = AgentId
    { unAgentId :: Integer
    } deriving (Show)

data CrTicket = CrTicket
    { tId           :: !AgentId
    , tCustomReport :: !CustomReport
    , tAttachment   :: !(Maybe Text)
    } deriving (Show)

instance ToJSON CrTicket where
  toJSON CrTicket {..} =
        object [ "ticket" .=
            object [ "type"          .= ("custom report" :: Text)
                    , "subject"       .= crSubject tCustomReport
                    , "description"   .= crDescription tCustomReport
                    , "requester_id"  .= unAgentId tId
                    , "assignee_id"   .= unAgentId tId
                    , "comment"       .=
                        object [ "type"    .= ("Attached logs" :: Text)
                                , "uploads" .= case tAttachment of
                                    Just token -> V.singleton token
                                    Nothing    -> V.empty
                                , "body" .= (crEmail tCustomReport
                                         <> "\n"
                                         <> crDescription tCustomReport)
                                ]
                    ]
        ]
