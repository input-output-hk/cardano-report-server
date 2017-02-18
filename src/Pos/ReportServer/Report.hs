{-# LANGUAGE ScopedTypeVariables #-}

-- | Log report datatype & related.

module Pos.ReportServer.Report
       ( ReportType(..)
       , supportedApps
       , ReportInfo(..)
       ) where

import           Data.Aeson                   (FromJSON (..), ToJSON (..),
                                               Value (Object, String), object, (.:), (.=))
import           Data.Aeson.Types             (typeMismatch)
import           Data.List                    (last)
import qualified Data.Text                    as T
import           Data.Time                    (UTCTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime,
                                               iso8601DateFormat, parseTimeM)
import           Data.Version                 (Version (..), parseVersion, showVersion)
import           Text.ParserCombinators.ReadP (readP_to_S)
import           Universum

-- | Type of report
data ReportType
    = RCrash Int
    | RError Text
    | RMisbehavior Text
    deriving (Show,Eq)

-- | Info medetadata sent with report
data ReportInfo = ReportInfo
    { rApplication :: Text
    , rVersion     :: Version
    , rBuild       :: Int
    , rOS          :: Text
    , rLogs        :: [Text]
    , rDate        :: UTCTime
    , rMagic       :: Int32
    , rReportType  :: ReportType
    } deriving (Show,Eq)

instance FromJSON ReportType where
    parseJSON (Object v) = (v .: "type") >>= \case
        String "crash" -> RCrash <$> v .: "errno"
        String "error" -> RError <$> v .: "message"
        String "misbehavior" -> RMisbehavior <$> v .: "reason"
        String unknown ->
            fail $ T.unpack $ "ReportType: report 'type' " <> unknown <> " is unknown"
        other  -> typeMismatch "ReportType.type: should be string" other
    parseJSON invalid    = typeMismatch "ReportType" invalid

-- Identity for text, more handy than disabling OverloadedStrings
idt :: Text -> Text
idt = identity

instance ToJSON ReportType where
    toJSON (RCrash errno) = object ["type" .= idt "crash", "errno" .= errno]
    toJSON (RError message) =
        object ["type" .= idt "error", "message" .= message]
    toJSON (RMisbehavior reason) =
        object ["type" .= idt "misbehavior", "reason" .= reason]

supportedApps :: [Text]
supportedApps = ["daedalus", "cardano-node"]

readVersion :: (ToString a) => a -> Maybe Version
readVersion (toString -> s) =
    case readP_to_S parseVersion s of
        [] -> Nothing
        xs -> Just $ fst $ last xs

-- YYYY-MM-DDTHH:MM:SS
iso8601DateTimeFormat :: [Char]
iso8601DateTimeFormat = iso8601DateFormat (Just "%H:%M:%S")

instance FromJSON ReportInfo where
    parseJSON (Object v) = do
        rApplication <- v .: "application"
        unless (rApplication `elem` supportedApps) $
            fail $ T.unpack $
            "ReportInfo.application should be in " <> T.intercalate ", " supportedApps
        (versionStr :: Text) <- v .: "version"
        rVersion <-
            maybe (fail $ "Can't read version: " <> show versionStr)
                  pure
                  (readVersion versionStr)
        rBuild <- v .: "build"
        rOS <- v .: "os"
        when (T.length rOS > 100) $ fail "OS field length cant be longer than 100 chars"
        rLogs <- v .: "logs"
        rDateStr <- v .: "date"
        rMagic <- v .: "magic"
        let failParseDate reason =
                fail $ "Can't parse date, should be in iso8601 format: " <>
                iso8601DateTimeFormat <> ", reason: " <> reason
        rDate <- either failParseDate pure $
            parseTimeM True defaultTimeLocale iso8601DateTimeFormat (T.unpack rDateStr)
        rReportType <- v .: "type"
        pure $ ReportInfo{..}
    parseJSON invalid    = typeMismatch "ReportInfo" invalid

instance ToJSON ReportInfo where
    toJSON ReportInfo {..} =
        object
            [ "application" .= rApplication
            , "version" .= showVersion rVersion
            , "build" .= rBuild
            , "os" .= rOS
            , "magic" .= rMagic
            , "logs" .= rLogs
            , "date" .= formatTime defaultTimeLocale iso8601DateTimeFormat rDate
            , "type" .= rReportType
            ]

{-
testReport = "{ \"application\": \"cardano-node\",\n\
             \  \"version\": \"0.0.1\",\n\
             \  \"build\": 1,\n\
             \  \"os\": \"Linux 4.9.2 NixOS\",\n\
             \  \"logs\": [ \"kek.log\" ],\n\
             \  \"date\": \"2017-02-01T11:18:51\",\n\
             \  \"type\": { \"type\": \"crash\", \"errno\": 5 } }\n"
-}
