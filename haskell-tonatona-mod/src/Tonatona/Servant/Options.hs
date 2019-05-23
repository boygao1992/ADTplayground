module Tonatona.Servant.Options where

import RIO
import Network.Wai.Handler.Warp (HostPreference)
import Options.Applicative (strOption, option, auto, long, metavar, value, help, flag, showDefault)

import Tonatona.Options.Parser (HasParser, parser)

class HasServantOptions options where
  servantOptionsL :: Lens' options ServantOptions

data ServantOptions = ServantOptions
  { host :: !Host
  , protocol :: !Protocol
  , port :: !Port
  , logging :: !Logging
  } deriving (Eq, Show)
instance HasParser ServantOptions where
  parser = ServantOptions <$> parser <*> parser <*> parser <*> parser
instance HasServantOptions ServantOptions where
  servantOptionsL = id

newtype Host = Host { unHost :: HostPreference }
  deriving newtype (Eq, Ord, IsString, Read, Show)
_host :: Lens' ServantOptions HostPreference
_host = lens (unHost . host) (\x y -> x { host = Host y })
instance HasParser Host where
  parser =
    strOption
    $ long "sh"
    <> metavar "HOST"
    <> value "127.0.0.1"
    <> showDefault
    <> help "set Servant Host"

newtype Protocol = Protocol { unProtocol :: Text }
  deriving newtype (Eq, Ord, IsString, Read, Show)
_protocol :: Lens' ServantOptions Text
_protocol = lens (unProtocol . protocol) (\x y -> x { protocol = Protocol y })
instance HasParser Protocol where
  parser =
    strOption
    $ long "sp"
    <> metavar "PROTOCOL"
    <> value (Protocol "http")
    <> showDefault
    <> help "set Servant Protocol"

newtype Port = Port { unPort :: Int }
  deriving newtype (Eq, Ord, Read, Show)
_port :: Lens' ServantOptions Int
_port = lens (unPort . port) (\x y -> x { port = Port y })
instance HasParser Port where
  parser =
    option auto
    $ long "sP"
    <> metavar "PORT"
    <> value (Port 8000)
    <> showDefault
    <> help "set Servant Port"

newtype Logging = Logging { unLogging :: Bool }
  deriving newtype (Eq, Ord, Read, Show)
_logging :: Lens' ServantOptions Bool
_logging = lens (unLogging . logging) (\x y -> x { logging = Logging y })
instance HasParser Logging where
  parser = Logging <$>
    flag True False
    ( long "sL"
    <> help "disable Servant Request Logger Middleware"
    )
