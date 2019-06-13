module Main (main) where

import RIO
import Shopify.TestApp.Types
import Shopify.TestApp.Run (run)

import Tonatona.WithResource
import Tonatona.Options.Parser

main :: IO ()
main = do
  (options :: Options) <- parseOptions
  runWithResource options (\resource -> runRIO resource run)

