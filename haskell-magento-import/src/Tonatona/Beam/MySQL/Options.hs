module Tonatona.Beam.MySQL.Options where

import RIO
import Options.Applicative (strOption, option, auto, long, metavar, value, help, showDefault)
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

class HasBeamMySQLOptions options where
  beamMySQLOptionsL :: Lens' options BeamMySQLOptions

data BeamMySQLOptions = BeamMySQLOptions
  { host :: !Host
  , port :: !Port
  , user :: !User
  , password :: !Password
  , database :: !Database
  } deriving (Eq, Read, Show)
instance HasParser BeamMySQLOptions where
  parser = BeamMySQLOptions <$> parser <*> parser <*> parser <*> parser <*> parser
instance HasBeamMySQLOptions BeamMySQLOptions where
  beamMySQLOptionsL = id

newtype Host = Host { unHost :: String }
  deriving newtype (Eq, IsString, Read, Show)
_host :: Lens' BeamMySQLOptions String
_host = lens (unHost . host) (\x y -> x { host = Host y })
instance HasParser Host where
  parser =
    strOption
    $ long "bmh"
    <> metavar "HOST"
    <> value (Host "localhost")
    <> showDefault
    <> help "set Beam MySQL Host"

newtype Port = Port { unPort :: Word16 }
  deriving newtype (Eq, Read, Show)
_port :: Lens' BeamMySQLOptions Word16
_port = lens (unPort . port) (\x y -> x { port = Port y })
instance HasParser Port where
  parser =
    option auto
    $ long "bmP"
    <> metavar "PORT"
    <> value (Port 3306)
    <> showDefault
    <> help "set Beam MySQL Port"

newtype User = User { unUser :: String }
  deriving newtype (Eq, IsString, Read, Show)
_user :: Lens' BeamMySQLOptions String
_user = lens (unUser . user) (\x y -> x { user = User y })
instance HasParser User where
  parser
    = strOption
    $ long "bmu"
    <> metavar "USER"
    <> value (User "root")
    <> showDefault
    <> help "set Beam MySQL User"

newtype Password = Password { unPassword :: String }
  deriving newtype (Eq, IsString, Read, Show)
_password :: Lens' BeamMySQLOptions String
_password = lens (unPassword . password) (\x y -> x { password = Password y })
instance HasParser Password where
  parser
    = strOption
    $ long "bmp"
    <> metavar "PASSWORD"
    <> value (Password "")
    <> showDefault
    <> help "set Beam MySQL Password"

newtype Database = Database { unDatabase :: String }
  deriving newtype (Eq, IsString, Read, Show)
_database :: Lens' BeamMySQLOptions String
_database = lens (unDatabase . database) (\x y -> x { database = Database y })
instance HasParser Database where
  parser
    = strOption
    $ long "bmD"
    <> metavar "DATABASE"
    <> help "set Beam MySQL Database name"
