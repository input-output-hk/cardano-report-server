{-# LANGUAGE OverloadedStrings #-}

-- | Types for sending data to zendesk.
module Pos.ForwardClient.Types
    ( CustomReport(..)
    , Agent(..)
    , Logs
    , Token
    , AgentId(..)
    , CrTicket(..)
    ) where

import           Universum

import           Data.Aeson (ToJSON (..), Value (Array), object, (.=))
import qualified Data.Vector as V

data CustomReport = CustomReport
    { crEmail       :: !Text
    , crSubject     :: !Text
    , crDescription :: !Text
    } deriving (Show)

data Agent = Agent
    { aEmail   :: !Text
    , apiToken :: !Token
    } deriving (Show)

type Logs = [(FilePath, LByteString)]

type Token = Text

newtype AgentId = AgentId
    { unAgentId :: Integer
    } deriving (Show)

data CrTicket = CrTicket
    { tId           :: !AgentId
    , tCustomReport :: !CustomReport
    , tAttachment   :: !Token
    } deriving (Show)

instance ToJSON CrTicket where
  toJSON CrTicket {..} =
      object [ "ticket" .=
          object [ "type"          .= ("custom report" :: Text)
                 , "subject"       .= crSubject tCustomReport
                 , "description"   .= crDescription tCustomReport
                 , "requester_id"  .= unAgentId tId
                 , "assignee_id"   .= unAgentId tId
                 , "custom_fields" .= Array (V.fromList $ one $ object
                                             [ "id" .= (1 :: Integer)
                                             , "value" .= crEmail tCustomReport])
                 , "comment"       .=
                      object [ "type"    .= ("Attached logs" :: Text)
                             , "uploads" .= V.singleton tAttachment
                             ]
                 ]
      ]
