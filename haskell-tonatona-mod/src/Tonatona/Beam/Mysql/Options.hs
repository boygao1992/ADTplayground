module Tonatona.Beam.Mysql.Options where

import RIO
import Options.Applicative (strOption, option, auto, long, metavar, value, help)
import Tonatona.Options.Parser (HasParser, parser)

{-
data ConnectInfo = ConnectInfo {
      connectHost :: String
    , connectPort :: Word16
    , connectUser :: String
    , connectPassword :: String
    , connectDatabase :: String
    , connectOptions :: [Option]
    , connectPath :: FilePath
    , connectSSL :: Maybe SSLInfo
} deriving (Eq, Read, Show, Typeable)

data SSLInfo = SSLInfo {
      sslKey :: FilePath
    , sslCert :: FilePath
    , sslCA :: FilePath
    , sslCAPath :: FilePath
    , sslCiphers :: String -- ^ Comma-separated list of cipher names.
} deriving (Eq, Read, Show, Typeable)
-}

class HasBeamMysqlOptions options where
  beamMysqlOptionsL :: Lens' options BeamMysqlOptions

data BeamMysqlOptions = BeamMysqlOptions
  { host :: !Host
  , port :: !Port
  , user :: !User
  , password :: !Password
  , database :: !Database
  } deriving (Eq, Read, Show)
instance HasParser BeamMysqlOptions where
  parser = BeamMysqlOptions <$> parser <*> parser <*> parser <*> parser <*> parser
instance HasBeamMysqlOptions BeamMysqlOptions where
  beamMysqlOptionsL = id

newtype Host = Host { unHost :: String }
  deriving newtype (Eq, IsString, Read, Show)
_host :: Lens' BeamMysqlOptions String
_host = lens (unHost . host) (\x y -> x { host = Host y })
instance HasParser Host where
  parser =
    strOption
    $ long "bmh"
    <> metavar "HOST"
    <> value (Host "localhost")
    <> help "Beam Mysql Host"

newtype Port = Port { unPort :: Word16 }
  deriving newtype (Eq, Read, Show)
_port :: Lens' BeamMysqlOptions Word16
_port = lens (unPort . port) (\x y -> x { port = Port y })
instance HasParser Port where
  parser =
    option auto
    $ long "bmP"
    <> metavar "PORT"
    <> value (Port 3306)
    <> help "Beam Mysql Port"

newtype User = User { unUser :: String }
  deriving newtype (Eq, IsString, Read, Show)
_user :: Lens' BeamMysqlOptions String
_user = lens (unUser . user) (\x y -> x { user = User y })
instance HasParser User where
  parser
    = strOption
    $ long "bmu"
    <> metavar "USER"
    <> value (User "root")
    <> help "Beam Mysql User"

newtype Password = Password { unPassword :: String }
  deriving newtype (Eq, IsString, Read, Show)
_password :: Lens' BeamMysqlOptions String
_password = lens (unPassword . password) (\x y -> x { password = Password y })
instance HasParser Password where
  parser
    = strOption
    $ long "bmp"
    <> metavar "PASSWORD"
    <> value (Password "root")
    <> help "Beam Mysql Password"

newtype Database = Database { unDatabase :: String }
  deriving newtype (Eq, IsString, Read, Show)
_database :: Lens' BeamMysqlOptions String
_database = lens (unDatabase . database) (\x y -> x { database = Database y })
instance HasParser Database where
  parser
    = strOption
    $ long "bmD"
    <> metavar "DATABASE"
    <> help "Beam Mysql Test"
