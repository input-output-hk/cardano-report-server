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

type Knowledgebase = [[BS.ByteString]]
type Logs          = LByteString

grabLogs :: FilePath -> IO LByteString
grabLogs path = BSL.readFile path
              <&> decompressLogs

decompressLogs :: LByteString -> LByteString
decompressLogs zipFile = zipFile
                       & toArchive
                       & zEntries
                       <&> fromEntry
                       & BSL.concat

grabKnowledgebase :: FilePath -> IO Knowledgebase
grabKnowledgebase path = do
    parseResult <- P.parseFromFile csvFile path
    case parseResult of
        Left _ -> error $ "CSV Parsing failed."
        Right (header:list) -> pure $ (fmap . fmap) encodeUtf8 list

analyse :: LByteString -> Knowledgebase -> Knowledgebase
analyse rawLogs base = let logs = decompressLogs rawLogs
                       in  flip filter base $ \(h:_) -> logs =~ h