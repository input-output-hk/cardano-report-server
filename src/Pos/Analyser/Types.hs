{-# LANGUAGE OverloadedStrings #-}

module Pos.Analyser.Types where

import Universum

import           Data.Aeson (ToJSON (..), object, (.=))

import           Data.ByteString (ByteString)
import qualified Data.Vector as V

type Logs             = LByteString
type CompressedLogs   = LByteString

newtype Knowledgebase = Knowledgebase { getKnowledgebase :: [[ByteString]] }
  deriving (Eq, Show)

instance ToJSON Knowledgebase where
    toJSON Knowledgebase {..} =
        let issues = (fmap . fmap) decodeUtf8 getKnowledgebase :: [[Text]]
        in  object [ "knowledgebase" .= V.fromList issues ]