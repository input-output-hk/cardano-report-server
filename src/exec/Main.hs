{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where


import           Universum

import qualified Network.Wai.Handler.Warp as Warp
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)

import           Pos.ForwardClient.Client (ZendeskParams (..), getAgentID)
import           Pos.ForwardClient.Types (mkAgent)
import           Pos.ReportServer.FileOps (initHolder)
import           Pos.ReportServer.Server (ServerContext (..), limitBodySize, reportServerApp)

import           Options (Opts (..), getOptions)

main :: IO ()
main = do
    a@Opts{..} <- getOptions

    -- compatibility issues with the old universum
    let pt (t :: Text) = putStr t
    let ptL (t :: Text) = putStrLn t

    ptL $ "Started with options: " <> show a

    pt "Reading/creating holder folder..."
    holder <- initHolder logsDir
    ptL "done"

    agent <- mapM mkAgent zdAgent

    scZendeskParams <- forM agent $ \za -> do
          pt "Authenticating in zendesk..."
          !agentID <- getAgentID za
          ptL "done"
          return $ ZendeskParams za agentID zdSendLogs

    let scStoreCustomReports = storeCustomReports
    let scLogsHolder = holder
    let sc = ServerContext {..}

    ptL "Launching server"
    let application = reportServerApp sc
    Warp.run port $ logStdoutDev $ limitBodySize sizeLimit application
