module Tonatona.Servant.Options where

import RIO
import Options.Applicative (strOption, option, auto, long, metavar, value, help)
import Tonatona.Options.Parser (HasParser, parser)

class HasServantOptions env where
  servantOptions :: Lens' env ServantOptions

data ServantOptions = ServantOptions
  { host :: !Host
  , protocol :: !Protocol
  , port :: !Port
  } deriving (Eq, Show)
instance HasParser ServantOptions where
  parser = ServantOptions <$> parser <*> parser <*> parser
instance HasServantOptions ServantOptions where
  servantOptions = id

newtype Host = Host { unHost :: Text }
  deriving newtype (Eq, Ord, IsString, Read, Show)
_host :: Lens' ServantOptions Text
_host = lens (unHost . host) (\x y -> x { host = Host y })
instance HasParser Host where
  parser =
    strOption
    $ long "host"
    <> metavar "HOST"
    <> value (Host "localhost")
    <> help "Servant Host"

newtype Protocol = Protocol { unProtocol :: Text }
  deriving newtype (Eq, Ord, IsString, Read, Show)
_protocol :: Lens' ServantOptions Text
_protocol = lens (unProtocol . protocol) (\x y -> x { protocol = Protocol y })
instance HasParser Protocol where
  parser =
    strOption
    $ long "protocol"
    <> metavar "PROTOCOL"
    <> value (Protocol "http")
    <> help "Servant Protocol"

newtype Port = Port { unPort :: Int }
  deriving newtype (Eq, Ord, Read, Show)
_port :: Lens' ServantOptions Int
_port = lens (unPort . port) (\x y -> x { port = Port y })
instance HasParser Port where
  parser =
    option auto
    $ long "port"
    <> metavar "PORT"
    <> value (Port 8000)
    <> help "Servant Port"
