{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Universum
import qualified Web.Scotty.Trans                     as S

import           Options                              (Opts (..), getOptions)
import           Pos.ReportServer.FileOps             (initHolder)
import           Pos.ReportServer.Server              (reportServerApp)


main :: IO ()
main = do
    a@Opts{..} <- getOptions
    putText $ "Started with options: " <> show a
    holder <- initHolder logsDir
    putText "Successfully created holder"

    putText "Launching server..."
    application <- S.scottyAppT liftIO $ reportServerApp holder
    Warp.run port $ logStdoutDev application
