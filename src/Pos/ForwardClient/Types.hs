{-# LANGUAGE OverloadedStrings #-}

-- | Types for sending data to zendesk.
module Pos.ForwardClient.Types
    ( CustomReport(..)
    , Agent(..)
    , Logs
    , Token
    , CrTicket(..)
    ) where

import           Universum

import           Data.Aeson (ToJSON (..), Value (Array), object, (.=))
import qualified Data.Vector as V

data CustomReport = CustomReport
    { crEmail       :: Text
    , crSubject     :: Text
    , crDescription :: Text
    } deriving (Show)

data Agent = Agent
    { aEmail   :: Text
    , apiToken :: Token
    , aId      :: Integer
    } deriving (Show)

type Logs = [(FilePath, LByteString)]

type Token = Text

data CrTicket = CrTicket
    { tAgent        :: Agent
    , tCustomReport :: CustomReport
    , tAttachment   :: Token
    } deriving (Show)

instance ToJSON CrTicket where
  toJSON CrTicket {..} =
      object [ "ticket" .=
          object [ "type"          .= ("custom report" :: Text)
                 , "subject"       .= crSubject tCustomReport
                 , "description"   .= crDescription tCustomReport
                 , "requester_id"  .= aId tAgent
                 , "assignee_id"   .= aId tAgent
                 , "custom_fields" .= Array (V.fromList $ one $ object
                                             [ "id" .= (1 :: Integer)
                                             , "value" .= crEmail tCustomReport])
                 , "comment"       .=
                      object [ "type"    .= ("Attached logs" :: Text)
                             , "uploads" .= V.singleton tAttachment
                             ]
                 ]
      ]
