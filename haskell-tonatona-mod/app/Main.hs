module Main (main) where

import RIO
import Types
import Run (run)

import Tonatona.WithResource
import Tonatona.Options.Parser

main :: IO ()
main = do
  (options :: Options) <- parseOptions
  runWithResource options (\resource -> runRIO resource run)

