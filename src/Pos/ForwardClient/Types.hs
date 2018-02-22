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

import           Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.Vector as V

data CustomReport = CustomReport
    { crEmail       :: !Text
    , crSubject     :: !Text
    , crDescription :: !Text
    } deriving (Show)

data Agent = Agent
    { aEmail   :: !Text
    , apiToken :: !Token
    , aAccount :: !Text
    } deriving (Show)

type Logs = [(FilePath, LByteString)]

type Token = Text

newtype AgentId = AgentId
    { unAgentId :: Integer
    } deriving (Show)

data CrTicket = CrTicket
    { tId           :: !AgentId
    , tCustomReport :: !CustomReport
    , tAttachment   :: !(Maybe Token)
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
