{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

-- | Options parser

module Options
       ( Opts (..)
       , getOptions
       ) where

import           Options.Applicative (Parser, auto, execParserPure, fullDesc, handleParseResult,
                                      header, help, helper, idm, info, infoOption, long, metavar,
                                      option, prefs, progDesc, short, strOption, switch, value,
                                      (<**>))
import           System.Directory (getHomeDirectory)
import           System.FilePath ((</>))
import           Universum

import           Paths_cardano_report_server (version)
import           Pos.ForwardClient.Types (Agent (..))

data Opts = Opts
    { port               :: Int
    , logsDir            :: FilePath
    , sizeLimit          :: Word64
    , storeCustomReports :: Bool

    , zdAgent            :: Maybe Agent
    , zdSendLogs         :: Bool
    } deriving (Show)

optsParser :: FilePath -> Parser Opts
optsParser homeDir = do
    port <-
        option
            auto
            (long "port" <> short 'p' <> metavar "INTEGER" <> value 8000 <>
             help "Port server is running on")
    logsDir <-
        strOption
            (long "logsdir" <> metavar "FILEPATH" <>
             value (homeDir </> ".cardano-report-server") <>
             help "Directory server will be saving logs in")
    sizeLimit <-
        option
            auto
            (long "size-limit" <> metavar "BYTES" <> value (25 * 1024 * 1024) <>
             help
                 "Maximum body size allowed (will send 413 responses if bigger)")
    storeCustomReports <-
        switch
            (long "store-custom" <> help "Store custom reports")

    zdSendLogs <-
        switch
            (long "zd-send-logs" <> help "Send logs from custom reports to Zendesk")

    zdAgent <- optional parseAgentOpts

    pure Opts {..}

parseAgentOpts :: Parser Agent
parseAgentOpts = Agent <$> email <*> token <*> account
  where
    email   = fromString <$> strOption
              (long "zd-email" <> metavar "STRING" <>
               help "Email to access zendesk")
    token   = fromString <$> strOption
              (long "zd-token" <> metavar "STRING" <>
               help "Zendesk api token")
    account = fromString <$> strOption
              (long "zd-account" <> metavar "NAME" <>
               help "Zendesk account name (first part of account URL)")

getOptions :: IO Opts
getOptions = do
    homeDir <- getHomeDirectory
    getArgs >>= handleParseResult . execParserPure (prefs idm) (parser homeDir)
  where
    parser homeDir =
        info (optsParser homeDir <**> helper <**> versionHelper) desc
    desc =
        fullDesc <> header "CardanoSL report server" <>
        progDesc "CardanoSL reporting server daemon"
    versionHelper =
        infoOption
            ("cardano-report-server version " <> show version)
            (long "version" <> help "Show version")
