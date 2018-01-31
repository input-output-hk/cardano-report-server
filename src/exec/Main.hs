{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Network.Wai.Handler.Warp as Warp
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Universum

import           Pos.ForwardClient.Client (getAgentID)
import           Pos.ReportServer.FileOps (initHolder)
import           Pos.ReportServer.Server (limitBodySize, reportServerApp)

import           Options (Opts (..), getOptions)

main :: IO ()
main = do
    a@Opts{..} <- getOptions
    putText $ "Started with options: " <> show a
    holder <- initHolder logsDir
    putText "Successfully created holder"

    putText "Launching server..."

    putText "Authenticating in zendesk..."
    agentID <- liftIO $ getAgentID zendeskAgent
    putText "Authenticating in zendesk... done"

    let application = reportServerApp holder zendeskAgent agentID
    Warp.run port $ logStdoutDev $ limitBodySize sizeLimit $ application
