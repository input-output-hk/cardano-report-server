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

-- | Type of report.
data ReportType
    = RCrash Int
    -- ^ This type is used only to report crash of application.
    | RError Text
    -- ^ The type of report used when a local error (most likely
    -- assertion violation or anything else that indicates a bug)
    -- happens. «Local» means that error most likely affects only one
    -- node for which this error happened.
    | RMisbehavior { rmIsCritical  :: Bool
                   -- ^ Whether misbehavior can break the system and
                   -- must be addressed ASAP.  Example of critical
                   -- misbehavior: chain quality is closed to
                   -- 50%. Example of non-critical misbehavior: fork
                   -- happened in bootstrap era. The latter should be
                   -- investigated, but doesn't mean that the system's
                   -- operability is threatened.
                   , rmDescription :: Text
                   -- ^ What exactly is suspicious\/wrong.
                    }
    -- ^ This type of report indicates global problems which most
    -- likely affect all nodes.
    | RInfo Text
    -- ^ The type of report used to send statistical or any other
    -- useful information and doesn't indicate anything
    -- bad\/strange\/suspicious.
    deriving (Show, Eq)

-- | Metadata sent with report.
data ReportInfo = ReportInfo
    { rApplication :: Text
      -- ^ Application name, e.g. "cardano-explorer" or "deadalus".
    , rVersion     :: Version
      -- ^ Application version.
    , rBuild       :: Text
      -- ^ Build information.
    , rOS          :: Text
      -- ^ OS information.
    , rDate        :: UTCTime
      -- ^ Date report was created on.
    , rMagic       :: Int32
      -- ^ Cluster magic.
    , rReportType  :: ReportType
      -- ^ Type of report.
    } deriving (Show,Eq)

instance FromJSON ReportType where
    parseJSON (Object v) = (v .: "type") >>= \case
        String "crash" -> RCrash <$> v .: "errno"
        String "error" -> RError <$> v .: "message"
        String "misbehavior" -> RMisbehavior <$> v .: "isCritical" <*> v .: "reason"
        String "info" -> RInfo <$> v .: "description"
        String unknown ->
            fail $ toString $ "ReportType: report 'type' " <> unknown <> " is unknown"
        other  -> typeMismatch "ReportType.type: should be string" other
    parseJSON invalid    = typeMismatch "ReportType" invalid

-- Identity for text, more handy than disabling OverloadedStrings
idt :: Text -> Text
idt = identity

instance ToJSON ReportType where
    toJSON (RCrash errno) = object ["type" .= idt "crash", "errno" .= errno]
    toJSON (RError message) =
        object ["type" .= idt "error", "message" .= message]
    toJSON (RMisbehavior isCritical reason) =
        object
            [ "type" .= idt "misbehavior"
            , "isCritical" .= isCritical
            , "reason" .= reason
            ]
    toJSON (RInfo descr) = object ["type" .= idt "info", "description" .= descr]

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
        let int2Text :: Int -> Text
            int2Text = fromString . show
        rBuild <- v .: "build" <|> (int2Text <$> v .: "build")
        when (T.length rBuild > 100) $ fail "Build field length can't be longer than 100 chars"
        rOS <- v .: "os"
        when (T.length rOS > 100) $ fail "OS field length can't be longer than 100 chars"
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
            , "date" .= formatTime defaultTimeLocale iso8601DateTimeFormat rDate
            , "type" .= rReportType
            ]

{-
testReport = "{ \"application\": \"cardano-node\",\n\
             \  \"version\": \"0.0.1\",\n\
             \  \"build\": "1",\n\
             \  \"os\": \"Linux 4.9.2 NixOS\",\n\
             \  \"logs\": [ \"kek.log\" ],\n\
             \  \"date\": \"2017-02-01T11:18:51\",\n\
             \  \"type\": { \"type\": \"crash\", \"errno\": 5 } }\n"
-}
