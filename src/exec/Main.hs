{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where


import           Universum

import qualified Network.Wai.Handler.Warp as Warp
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)

import           Pos.ForwardClient.Client (getAgentID)
import           Pos.ReportServer.FileOps (initHolder)
import           Pos.ReportServer.Server (limitBodySize, reportServerApp)

import           Options (Opts (..), getOptions)

main :: IO ()
main = do
    a@Opts{..} <- getOptions
    putTextLn $ "Started with options: " <> show a

    putText "Reading/creating holder folder..."
    holder <- initHolder logsDir
    putTextLn "done"

    putText "Authenticating in zendesk..."
    !agentID <- getAgentID zendeskAgent
    putTextLn "done"

    putTextLn "Launching server"

    let application = reportServerApp holder zendeskAgent agentID
    Warp.run port $ logStdoutDev $ limitBodySize sizeLimit $ application
