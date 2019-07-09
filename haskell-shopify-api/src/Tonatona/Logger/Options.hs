module Tonatona.Logger.Options where

import RIO
import Options.Applicative (long, help, flag, switch)

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
    flag Development Production
    $ long "prod"
    <> help "run Application in Production mode"

newtype Verbose = Verbose { unVerbose :: Bool }
  deriving newtype (Eq, Ord, Read, Show)
_verbose :: Lens' LoggerOptions Verbose
_verbose = lens verbose (\x y -> x { verbose = y })
instance HasParser Verbose where
  parser = Verbose <$>
    switch
    ( long "verbose"
    <> help "set Logger to Verbose mode"
    )
