module Tonatona.Beam.Postgres.Options where

import RIO
import Options.Applicative (strOption, option, auto, long, metavar, value, help)

import Tonatona.Options.Parser (HasParser, parser)

-- Option Parser

class HasBeamPostgresOptions options where
  beamPostgresOptionsL :: Lens' options BeamPostgresOptions

data BeamPostgresOptions = BeamPostgresOptions
  { host :: !Host
  , user :: !User
  , password :: !Password
  , database :: !Database
  , port :: !Port
  } deriving (Eq, Read, Show)
instance HasParser BeamPostgresOptions where
  parser = BeamPostgresOptions <$> parser <*> parser <*> parser <*> parser <*> parser
instance HasBeamPostgresOptions BeamPostgresOptions where
  beamPostgresOptionsL = id

newtype Host = Host { unHost :: String }
  deriving newtype (Eq, Ord, IsString, Read, Show)
_host :: Lens' BeamPostgresOptions String
_host = lens (unHost . host) (\x y -> x { host = Host y })
instance HasParser Host where
  parser =
    strOption
    $ long "bph"
    <> metavar "HOST"
    <> value (Host "localhost")
    <> help "set Beam Postgres Host"

newtype User = User { unUser :: String }
  deriving newtype (Eq, Ord, IsString, Read, Show)
_user :: Lens' BeamPostgresOptions String
_user = lens (unUser . user) (\x y -> x { user = User y })
instance HasParser User where
  parser =
    strOption
    $ long "bpu"
    <> metavar "USER"
    <> value (User "root")
    <> help "set Beam Postgres Username"

newtype Password = Password { unPassword :: String }
  deriving newtype (Eq, Ord, IsString, Read, Show)
_password :: Lens' BeamPostgresOptions String
_password = lens (unPassword . password) (\x y -> x { password = Password y })
instance HasParser Password where
  parser =
    strOption
    $ long "bpp"
    <> metavar "PASSWORD"
    <> value (Password "root")
    <> help "set Beam Postgres Password"

newtype Database = Database { unDatabase :: String }
  deriving newtype (Eq, Ord, IsString, Read, Show)
_database :: Lens' BeamPostgresOptions String
_database = lens (unDatabase . database) (\x y -> x { database = Database y })
instance HasParser Database where
  parser =
    strOption
    $ long "bpD"
    <> metavar "DB"
    <> help "set Beam Postgres Database name"

newtype Port = Port { unPort :: Word16 }
  deriving newtype (Eq, Ord, Read, Show)
_port :: Lens' BeamPostgresOptions Word16
_port = lens (unPort . port) (\x y -> x { port = Port y })
instance HasParser Port where
  parser =
    option auto
    $ long "bpP"
    <> metavar "PORT"
    <> value (Port 3306)
    <> help "set Beam Postgres Port"
