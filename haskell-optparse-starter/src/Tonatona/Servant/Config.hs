module Tonatona.Servant.Config where

import Import
import Options.Applicative
import Tonatona.Options.Parser

class HasServantConfig env where
  servantConfig :: Lens' env ServantConfig

data ServantConfig = ServantConfig
  { host :: Host
  , protocol :: Protocol
  , port :: Port
  } deriving (Eq, Show)
instance HasParser ServantConfig where
  parser = ServantConfig <$> parser <*> parser <*> parser
instance HasServantConfig ServantConfig where
  servantConfig = id

newtype Host = Host { unHost :: Text }
  deriving newtype (Eq, Ord, IsString, Read, Show)

instance HasParser Host where
  parser =
    strOption
    $ long "host"
    <> metavar "HOST"
    <> value (Host "localhost")
    <> help "Servant Host name to serve"

newtype Protocol = Protocol { unProtocol :: Text }
  deriving newtype (Eq, Ord, IsString, Read, Show)

instance HasParser Protocol where
  parser =
    strOption
    $ long "protocol"
    <> metavar "PROTOCOL"
    <> value (Protocol "http")
    <> help "Servant Protocol to serve"

newtype Port = Port { unPort :: Int }
  deriving newtype (Eq, Ord, Read, Show)

instance HasParser Port where
  parser =
    option auto
    $ long "port"
    <> metavar "PORT"
    <> value (Port 8000)
    <> help "Servant Port to serve"
