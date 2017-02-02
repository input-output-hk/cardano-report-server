{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

-- | Options parser

module Options
       ( Opts (..)
       , getOptions
       ) where

import           Data.Char                   (toLower, toUpper)
import           Options.Applicative         (ReadM, eitherReader)
import           Options.Applicative.Simple  (Parser, auto, help, long, metavar, option,
                                              short, simpleOptions, simpleVersion,
                                              strOption, value)
import           System.Directory            (getHomeDirectory)
import           System.FilePath             ((</>))
import           System.Wlog.Severity        (Severity (..))
import qualified Text.Parsec                 as P
import           Universum

import           Paths_cardano_report_server (version)

data Opts = Opts
    { port     :: Int
    , logsDir  :: FilePath
    , severity :: Severity
    } deriving (Show)

fromParsec :: P.Parsec [Char] () a -> ReadM a
fromParsec parser =
    eitherReader $ either (Left . show) Right . P.parse parser "<CLI options>"

severityParser :: P.Parsec [Char] () Severity
severityParser =
    foldl1 (<|>) $ map toParser $ [Debug, Info, Notice, Warning, Error]
  where

    toParser x = string' (show x) $> x
    string' t =
        P.string t <|> P.string (map toLower t) <|> P.string (map toUpper t)

optsParser :: FilePath -> Parser Opts
optsParser homeDir =
    Opts <$>
    option
        auto
        (long "port" <> short 'p' <> metavar "INTEGER" <> value 8000 <>
         help "Port server is running on") <*>
    strOption
        (long "logsdir" <> metavar "FILEPATH" <>
         value (homeDir </> ".cardano-report-server") <>
         help "Directory server will be saving logs in") <*>
    option
        (fromParsec severityParser)
        (long "severity" <> metavar "SEVERITY" <> value Info <>
         help "Logging severity")

getOptions :: IO Opts
getOptions = do
    homeDir <- getHomeDirectory
    (res, ()) <-
        simpleOptions
            ("cardano-report-server, " <> $(simpleVersion version))
            "CardanoSL report server"
            "CardanoSL reporting server daemon"
            (optsParser homeDir)
            empty
    return res
