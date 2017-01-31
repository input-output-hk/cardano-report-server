module Main (main) where

import           Universum

import           Options   (Opts (..), getOptions)

main :: IO ()
main = do
    a@Opts{..} <- getOptions
    print a
