module Tonatona.Beam.Postgresql.Options where

import RIO
import Options.Applicative (strOption, option, auto, long, metavar, value, help)
import Tonatona.Options.Parser (HasParser, parser)

class HasBeamPostgresqlOptions env where
  beamPostgresqlOptions :: Lens' env BeamPostgresqlOptions

data BeamPostgresqlOptions = BeamPostgresqlOptions
  { host :: !Host
  , user :: !User
  , password :: !Password
  , database :: !Database
  , port :: !Port
  } deriving (Eq, Read, Show)
instance HasParser BeamPostgresqlOptions where
  parser = BeamPostgresqlOptions <$> parser <*> parser <*> parser <*> parser <*> parser
instance HasBeamPostgresqlOptions BeamPostgresqlOptions where
  beamPostgresqlOptions = id

newtype Host = Host { unHost :: String }
  deriving newtype (Eq, Ord, IsString, Read, Show)
_host :: Lens' BeamPostgresqlOptions String
_host = lens (unHost . host) (\x y -> x { host = Host y })
instance HasParser Host where
  parser =
    strOption
    $ long "bph"
    <> metavar "HOST"
    <> value (Host "localhost")
    <> help "Beam Postgresql Host"

newtype User = User { unUser :: String }
  deriving newtype (Eq, Ord, IsString, Read, Show)
_user :: Lens' BeamPostgresqlOptions String
_user = lens (unUser . user) (\x y -> x { user = User y })
instance HasParser User where
  parser =
    strOption
    $ long "bpu"
    <> metavar "USER"
    <> value (User "root")
    <> help "Beam Postgresql Username"

newtype Password = Password { unPassword :: String }
  deriving newtype (Eq, Ord, IsString, Read, Show)
_password :: Lens' BeamPostgresqlOptions String
_password = lens (unPassword . password) (\x y -> x { password = Password y })
instance HasParser Password where
  parser =
    strOption
    $ long "bpp"
    <> metavar "PASSWORD"
    <> value (Password "root")
    <> help "Beam Postgresql Password"

newtype Database = Database { unDatabase :: String }
  deriving newtype (Eq, Ord, IsString, Read, Show)
_database :: Lens' BeamPostgresqlOptions String
_database = lens (unDatabase . database) (\x y -> x { database = Database y })
instance HasParser Database where
  parser =
    strOption
    $ long "bpD"
    <> metavar "DB"
    <> help "Beam Postgresql Database name"

newtype Port = Port { unPort :: Word16 }
  deriving newtype (Eq, Ord, Read, Show)
_port :: Lens' BeamPostgresqlOptions Word16
_port = lens (unPort . port) (\x y -> x { port = Port y })
instance HasParser Port where
  parser =
    option auto
    $ long "bpP"
    <> metavar "PORT"
    <> value (Port 3306)
    <> help "Beam Postgresql Port"
