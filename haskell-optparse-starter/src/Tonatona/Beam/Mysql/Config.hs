module Tonatona.Beam.Mysql.Config where

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

class HasBeamMysqlConfig env where
  beamMysqlConfig :: Lens' env BeamMysqlConfig

data BeamMysqlConfig = BeamMysqlConfig
  { host :: Host
  , port :: Port
  , user :: User
  , password :: Password
  , database :: Database
  } deriving (Eq, Read, Show)
instance HasParser BeamMysqlConfig where
  parser = BeamMysqlConfig <$> parser <*> parser <*> parser <*> parser <*> parser
instance HasBeamMysqlConfig BeamMysqlConfig where
  beamMysqlConfig = id

newtype Host = Host { unHost :: String }
  deriving newtype (Eq, IsString, Read, Show)
instance HasParser Host where
  parser =
    strOption
    $ long "bmh"
    <> metavar "HOST"
    <> value (Host "localhost")
    <> help "Beam Mysql Host"

newtype Port = Port { unPort :: Word16 }
  deriving newtype (Eq, Read, Show)
instance HasParser Port where
  parser =
    option auto
    $ long "bmP"
    <> metavar "PORT"
    <> value (Port 3306)
    <> help "Beam Mysql Port"

newtype User = User { unUser :: String }
  deriving newtype (Eq, IsString, Read, Show)
instance HasParser User where
  parser
    = strOption
    $ long "bmu"
    <> metavar "USER"
    <> value (User "root")
    <> help "Beam Mysql User"

newtype Password = Password { unPassword :: String }
  deriving newtype (Eq, IsString, Read, Show)
instance HasParser Password where
  parser
    = strOption
    $ long "bmp"
    <> metavar "PASSWORD"
    <> value (Password "root")
    <> help "Beam Mysql Password"

newtype Database = Database { unDatabase :: String }
  deriving newtype (Eq, IsString, Read, Show)
instance HasParser Database where
  parser
    = strOption
    $ long "bmD"
    <> metavar "DATABASE"
    <> help "Beam Mysql Test"
