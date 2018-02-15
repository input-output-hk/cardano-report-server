{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where


import           Universum

import qualified Network.Wai.Handler.Warp as Warp
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)

import           Pos.ForwardClient.Client (getAgentID, ReportAppParams (..))
import           Pos.ReportServer.FileOps (initHolder)
import           Pos.ReportServer.Server (limitBodySize, reportServerApp)

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

    mAgentID <- forM zendeskAgent $ \za -> do
          pt "Authenticating in zendesk..."
          !agentID <- getAgentID za
          ptL "done"
          return (agentID, za)

    ptL "Launching server"
    let rap = fmap (\(agentID, za) -> ReportAppParams za agentID store sendLogs) mAgentID
    let application = reportServerApp holder rap
    Warp.run port $ logStdoutDev $ limitBodySize sizeLimit application
