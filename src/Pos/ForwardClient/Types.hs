{-# LANGUAGE OverloadedStrings #-}

module Pos.ForwardClient.Types where

import Prelude

import           Data.ByteString.Lazy as BSL
import           Data.Text
import qualified Data.Vector          as V
import           Data.Aeson           (ToJSON (..), Value (Array), object, (.=))

data CustomReport = CustomReport {
    crEmail       :: Text
  , crSubject     :: Text
  , crDescription :: Text
} deriving Show

data Agent = Agent {
    aEmail    :: Text
  , apiToken  :: Token
  , aId       :: Integer
} deriving Show

type Logs = [(FilePath, BSL.ByteString)]

type Token = Text

data CrTicket = CrTicket {
    tAgent        :: Agent
  , tCustomReport :: CustomReport
  , tAttachment   :: Token
} deriving Show

instance ToJSON CrTicket where
  toJSON CrTicket {..} =
    object [ "ticket" .=
      object [ "type"          .= ("custom report" :: Text)
             , "subject"       .= crSubject tCustomReport
             , "description"   .= crDescription tCustomReport
             , "requester_id"  .= aId tAgent
             , "assignee_id"   .= aId tAgent
             , "custom_fields" .= Array (V.fromList [ object [ "id" .= (1 :: Integer), "value" .= crEmail tCustomReport]])
             , "comment"       .=
                  object [ "type"    .= ("Attached logs" :: Text)
                         , "uploads" .= V.singleton tAttachment
                         ]
             ]
    ]