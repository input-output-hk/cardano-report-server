module Main (main) where

import           Universum

import           Options   (Opts (..), getNodeOptions)

main :: IO ()
main = do
    a@Opts{..} <- getNodeOptions
    print a
