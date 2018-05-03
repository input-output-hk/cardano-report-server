{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Pos.Analyser.Analysis (analyse
                             ,grabKnowledgebase
                             ,grabLogs) where

import Universum

import           Codec.Archive.Zip (toArchive, zEntries, fromEntry)

import           Data.CSV (csvFile)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import qualified Text.ParserCombinators.Parsec   as P
import           Text.Regex.PCRE ((=~))

import           Pos.Analyser.Types

grabLogs :: FilePath -> IO Logs
grabLogs path = BSL.readFile path
              <&> decompressLogs

decompressLogs :: CompressedLogs -> Logs
decompressLogs zipFile = zipFile
                       & toArchive
                       & zEntries
                       <&> fromEntry
                       & BSL.concat

grabKnowledgebase :: FilePath -> IO Knowledgebase
grabKnowledgebase path = do
    parseResult <- P.parseFromFile csvFile path
    case parseResult of
        Right (_:records) -> pure $ Knowledgebase $ (fmap . fmap) encodeUtf8 records
        _      -> error $ "CSV Parsing failed."

analyse :: CompressedLogs -> Knowledgebase -> Knowledgebase
analyse rawLogs Knowledgebase{..} =
    let logs = decompressLogs rawLogs
    in  Knowledgebase $ flip filter getKnowledgebase $ \(h:_) -> logs =~ h