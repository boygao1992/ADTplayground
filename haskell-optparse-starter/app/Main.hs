{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import Import
-- import Run
-- import RIO.Process
-- import Options.Applicative.Simple
-- import qualified Paths_haskell_optparse_starter

import Options.Applicative
import Prelude (print)
import Tonatona.Options.Parser
import Tonatona.Logger.Config
import Tonatona.Servant.Config
import Tonatona.Beam.Mysql.Config

-- main1 :: IO ()
-- main1 = do
--   (options, ()) <- simpleOptions
--     $(simpleVersion Paths_haskell_optparse_starter.version)
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

data AppConfig = AppConfig
  { appLogConfig :: LoggerConfig
  , appServantConfig :: ServantConfig
  , appBeamMysqlConfig :: BeamMysqlConfig
  }
  deriving (Eq, Show)

instance HasParser AppConfig where
  parser = AppConfig <$> parser <*> parser <*> parser

instance HasLoggerConfig AppConfig where
  loggerConfig = lens appLogConfig (\x y -> x { appLogConfig = y })
instance HasServantConfig AppConfig where
  servantConfig = lens appServantConfig (\x y -> x { appServantConfig = y })
instance HasBeamMysqlConfig AppConfig where
  beamMysqlConfig = lens appBeamMysqlConfig (\x y -> x { appBeamMysqlConfig = y })

dparser :: ParserInfo AppConfig
dparser = info (parser @AppConfig <**> helper) mempty

main :: IO ()
main = do
  config <- execParser dparser
  print config
