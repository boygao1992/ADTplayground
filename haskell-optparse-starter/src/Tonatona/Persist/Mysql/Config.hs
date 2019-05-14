module Tonatona.Persist.Mysql.Config where

import Import
import Options.Applicative
import Tonatona.Options.Parser

class HasPersistMysqlConfig env where
  persistMysqlConfig :: Lens' env PersistMysqlConfig

data PersistMysqlConfig = PersistMysqlConfig
  { host :: Host
  , user :: User
  , password :: Password
  , database :: Database
  , port :: Port
  } deriving (Eq, Read, Show)
instance HasParser PersistMysqlConfig where
  parser = PersistMysqlConfig <$> parser <*> parser <*> parser <*> parser <*> parser
instance HasPersistMysqlConfig PersistMysqlConfig where
  persistMysqlConfig = id

newtype Host = Host { unHost :: Text }
  deriving newtype (Eq, Ord, IsString, Read, Show)
instance HasParser Host where
  parser =
    strOption
    $ long "pmh"
    <> metavar "HOST"
    <> value (Host "localhost")
    <> help "Persist Mysql Host"

newtype User = User { unUser :: Text }
  deriving newtype (Eq, Ord, IsString, Read, Show)
instance HasParser User where
  parser =
    strOption
    $ long "pmu"
    <> metavar "USER"
    <> value (User "root")
    <> help "Persist Mysql Username"

newtype Password = Password { unPassword :: Text }
  deriving newtype (Eq, Ord, IsString, Read, Show)
instance HasParser Password where
  parser =
    strOption
    $ long "pmp"
    <> metavar "PASSWORD"
    <> value (Password "root")
    <> help "Persist Mysql Password"

newtype Database = Database { unDatabase :: Text }
  deriving newtype (Eq, Ord, IsString, Read, Show)
instance HasParser Database where
  parser =
    strOption
    $ long "pmD"
    <> metavar "DB"
    <> help "Persist Mysql Database name"

newtype Port = Port { unPort :: Int }
  deriving newtype (Eq, Ord, Read, Show)
instance HasParser Port where
  parser =
    option auto
    $ long "pmP"
    <> metavar "PORT"
    <> value (Port 3306)
    <> help "Persist Mysql Port"
