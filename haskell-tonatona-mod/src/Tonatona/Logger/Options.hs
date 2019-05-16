module Tonatona.Logger.Options where

import RIO
import Options.Applicative (option, auto, long, metavar, value, help)

import Tonatona.Options.Parser (HasParser, parser)

-- API

defaultWithLogFunc ::
  ( MonadUnliftIO m
  , HasLoggerOptions options
  )
  => options
  -> (LogFunc -> m a) -> m a
defaultWithLogFunc options action = do
  logOptions <- defaultLogOptions options
  withLogFunc logOptions action

defaultLogOptions ::
  ( MonadIO m
  , HasLoggerOptions options
  )
  => options
  -> m LogOptions
defaultLogOptions options = do
  let LoggerOptions { mode, verbose } = view loggerOptionsL options
  logOptionsHandle stderr $ defaultVerbosity mode verbose

defaultVerbosity :: DeployMode -> Verbose -> Bool
defaultVerbosity mode (Verbose v) =
  case (mode, v) of
    (Development, _) -> True
    _ -> v
-- Option Parser

class HasLoggerOptions options where
  loggerOptionsL :: Lens' options LoggerOptions

data LoggerOptions = LoggerOptions
  { mode :: !DeployMode
  , verbose :: !Verbose
  } deriving (Eq, Ord, Show, Read)
instance HasParser LoggerOptions where
  parser = LoggerOptions <$> parser <*> parser
instance HasLoggerOptions LoggerOptions where
  loggerOptionsL = id

data DeployMode
  = Development
  | Production
  deriving (Eq, Ord, Show, Read, Bounded, Enum)
_mode :: Lens' LoggerOptions DeployMode
_mode = lens mode (\x y -> x { mode = y })
instance HasParser DeployMode where
  parser =
    option auto
    $ long "env"
    <> metavar "ENV"
    <> value Development
    <> help "Application deployment mode to run"

newtype Verbose = Verbose { unVerbose :: Bool }
  deriving newtype (Eq, Ord, Read, Show)
_verbose :: Lens' LoggerOptions Bool
_verbose = lens (unVerbose . verbose) (\x y -> x { verbose = Verbose y })
instance HasParser Verbose where
  parser =
    option auto
    $ long "verbose"
    <> metavar "VERBOSE"
    <> value (Verbose False)
    <> help "Make the operation more talkative"
