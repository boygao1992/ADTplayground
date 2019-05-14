module Tonatona.Beam.Postgresql.Config where

import Import
import Options.Applicative
import Tonatona.Options.Parser

class HasBeamPostgresqlConfig env where
  beamPostgresqlConfig :: Lens' env BeamPostgresqlConfig

data BeamPostgresqlConfig = BeamPostgresqlConfig
  { host :: Host
  , user :: User
  , password :: Password
  , database :: Database
  , port :: Port
  } deriving (Eq, Read, Show)
instance HasParser BeamPostgresqlConfig where
  parser = BeamPostgresqlConfig <$> parser <*> parser <*> parser <*> parser <*> parser
instance HasBeamPostgresqlConfig BeamPostgresqlConfig where
  beamPostgresqlConfig = id

newtype Host = Host { unHost :: String }
  deriving newtype (Eq, Ord, IsString, Read, Show)
instance HasParser Host where
  parser =
    strOption
    $ long "bph"
    <> metavar "HOST"
    <> value (Host "localhost")
    <> help "Beam Postgresql Host"

newtype User = User { unUser :: String }
  deriving newtype (Eq, Ord, IsString, Read, Show)
instance HasParser User where
  parser =
    strOption
    $ long "bpu"
    <> metavar "USER"
    <> value (User "root")
    <> help "Beam Postgresql Username"

newtype Password = Password { unPassword :: String }
  deriving newtype (Eq, Ord, IsString, Read, Show)
instance HasParser Password where
  parser =
    strOption
    $ long "bpp"
    <> metavar "PASSWORD"
    <> value (Password "root")
    <> help "Beam Postgresql Password"

newtype Database = Database { unDatabase :: String }
  deriving newtype (Eq, Ord, IsString, Read, Show)
instance HasParser Database where
  parser =
    strOption
    $ long "bpD"
    <> metavar "DB"
    <> help "Beam Postgresql Database name"

newtype Port = Port { unPort :: Word16 }
  deriving newtype (Eq, Ord, Read, Show)
instance HasParser Port where
  parser =
    option auto
    $ long "bpP"
    <> metavar "PORT"
    <> value (Port 3306)
    <> help "Beam Postgresql Port"
