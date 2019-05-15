module Tonatona.Persist.Mysql.Options where

import RIO
import Options.Applicative (strOption, option, auto, long, metavar, value, help)
import Tonatona.Options.Parser (HasParser, parser)

class HasPersistMysqlOptions options where
  persistMysqlOptions :: Lens' options PersistMysqlOptions

data PersistMysqlOptions = PersistMysqlOptions
  { host :: !Host
  , user :: !User
  , password :: !Password
  , database :: !Database
  , port :: !Port
  } deriving (Eq, Read, Show)
instance HasParser PersistMysqlOptions where
  parser = PersistMysqlOptions <$> parser <*> parser <*> parser <*> parser <*> parser
instance HasPersistMysqlOptions PersistMysqlOptions where
  persistMysqlOptions = id

newtype Host = Host { unHost :: Text }
  deriving newtype (Eq, Ord, IsString, Read, Show)
_host :: Lens' PersistMysqlOptions Text
_host = lens (unHost . host) (\x y -> x { host = Host y })
instance HasParser Host where
  parser =
    strOption
    $ long "pmh"
    <> metavar "HOST"
    <> value (Host "localhost")
    <> help "Persist Mysql Host"

newtype User = User { unUser :: Text }
  deriving newtype (Eq, Ord, IsString, Read, Show)
_user :: Lens' PersistMysqlOptions Text
_user = lens (unUser . user) (\x y -> x { user = User y })
instance HasParser User where
  parser =
    strOption
    $ long "pmu"
    <> metavar "USER"
    <> value (User "root")
    <> help "Persist Mysql Username"

newtype Password = Password { unPassword :: Text }
  deriving newtype (Eq, Ord, IsString, Read, Show)
_password :: Lens' PersistMysqlOptions Text
_password = lens (unPassword . password) (\x y -> x { password = Password y })
instance HasParser Password where
  parser =
    strOption
    $ long "pmp"
    <> metavar "PASSWORD"
    <> value (Password "root")
    <> help "Persist Mysql Password"

newtype Database = Database { unDatabase :: Text }
  deriving newtype (Eq, Ord, IsString, Read, Show)
_database :: Lens' PersistMysqlOptions Text
_database = lens (unDatabase . database) (\x y -> x { database = Database y })
instance HasParser Database where
  parser =
    strOption
    $ long "pmD"
    <> metavar "DB"
    <> help "Persist Mysql Database name"

newtype Port = Port { unPort :: Int }
  deriving newtype (Eq, Ord, Read, Show)
_port :: Lens' PersistMysqlOptions Int
_port = lens (unPort . port) (\x y -> x { port = Port y })
instance HasParser Port where
  parser =
    option auto
    $ long "pmP"
    <> metavar "PORT"
    <> value (Port 3306)
    <> help "Persist Mysql Port"
