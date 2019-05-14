module Tonatona.Logger.Config where

import Import
import Options.Applicative
import Tonatona.Options.Parser

class HasLoggerConfig env where
  loggerConfig :: Lens' env LoggerConfig

data LoggerConfig = LoggerConfig
  { mode :: DeployMode
  , verbose :: Verbose
  } deriving (Eq, Ord, Show, Read)
instance HasParser LoggerConfig where
  parser = LoggerConfig <$> parser <*> parser
instance HasLoggerConfig LoggerConfig where
  loggerConfig = id

data DeployMode
  = Development
  | Production
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

instance HasParser DeployMode where
  parser =
    option auto
    $ long "env"
    <> metavar "ENV"
    <> value Development
    <> help "Application deployment mode to run"

newtype Verbose = Verbose { unVerbose :: Bool }
  deriving newtype (Eq, Ord, Read, Show)

instance HasParser Verbose where
  parser =
    option auto
    $ long "verbose"
    <> metavar "VERBOSE"
    <> value (Verbose False)
    <> help "Make the operation more talkative"
