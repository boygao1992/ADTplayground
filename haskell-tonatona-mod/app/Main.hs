module Main (main) where

import RIO
import Types
import Run
-- import RIO.Process
-- import Options.Applicative.Simple
-- import qualified Paths_haskell_tonatona_mod

import Tonatona.WithResource
import Tonatona.Options.Parser

main :: IO ()
main = do
  (options :: Options) <- parseOptions
  runWithResource options (\resource -> runRIO resource run)

-- main :: IO ()
-- main = do
--   (options, ()) <- simpleOptions
--     $(simpleVersion Paths_haskell_tonatona_mod.version)
--     "Header for command line arguments"
--     "Program description, also for command line arguments"
--     (Options
--        <$> switch ( long "verbose"
--                  <> short 'v'
--                  <> help "Verbose output?"
--                   )
--     )
--     empty
--   lo <- logOptionsHandle stderr (optionsVerbose options)
--   pc <- mkDefaultProcessContext
--   withLogFunc lo $ \lf ->
--     let app = App
--           { appLogFunc = lf
--           , appProcessContext = pc
--           , appOptions = options
--           }
--      in runRIO app run