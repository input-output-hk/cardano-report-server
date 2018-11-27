{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Types for sending data to zendesk.
module Pos.ForwardClient.Types
    ( CustomReport(..)
    , Agent(..)
    , AgentOptions(..)
    , Logs
    , Token (..)
    , mkAgent
    , getToken
    , AgentId(..)
    , CustomField (..)
    , CrTicket(..)
    ) where

import           Universum

import qualified Data.Text as T
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
  token <- Token . T.filter (/= '\n') <$> readFile (aoApiToken ao)
  return $ Agent
    { aEmail    = (aoEmail ao)
    , apiToken  = token
    , aAccount  = (aoAccount ao)
    }

getToken :: Token -> Text
getToken (Token token) = token

newtype AgentId = AgentId
    { unAgentId :: Integer
    } deriving (Show)

-- https://developer.zendesk.com/rest_api/docs/core/tickets#setting-custom-field-values
data CustomField = CustomField
    { id            :: !Int
    , value         :: !Text
    } deriving (Show)

instance ToJSON CustomField where
    toJSON CustomField {..} =
        object
            [ "id" .= id
            , "value" .= value
            ]

data CrTicket = CrTicket
    { tId           :: !AgentId
    , tCustomReport :: !CustomReport
    , tAttachment   :: !(Maybe Text)
    , tCustomFields :: ![CustomField]
    } deriving (Show)

instance ToJSON CrTicket where
  toJSON CrTicket {..} =
        object [ "ticket" .=
            object [ "type"         .= ("custom report" :: Text)
                    , "status"      .= ("new" :: Text)
                    , "subject"     .= crSubject tCustomReport
                    , "description" .= crDescription tCustomReport
                    , "comment"     .=
                        object [ "type"     .= ("Attached logs" :: Text)
                                , "uploads" .= case tAttachment of
                                    Just token -> V.singleton token
                                    Nothing    -> V.empty
                                , "body"    .= (crEmail tCustomReport
                                            <> "\n"
                                            <> crDescription tCustomReport)
                                ]
                    , "requester"    .=
                        object [ "name"  .= crEmail tCustomReport
                               , "email" .= crEmail tCustomReport
                               ]
                    , "submitter_id"  .= unAgentId tId
                    -- yeah, this is pretty weird.
                    , "fields"        .= tCustomFields
                    , "custom_fields" .= tCustomFields
                    ]
        ]
