module Tonatona.Persist.Postgresql.Config where

import RIO
import Options.Applicative (strOption, option, auto, long, metavar, value, help)
import Tonatona.Options.Parser (HasParser, parser)

class HasPersistPostgresqlConfig env where
  persistPostgresqlConfig :: Lens' env PersistPostgresqlConfig

data PersistPostgresqlConfig = PersistPostgresqlConfig
  { connectionString :: ConnectionString
  , poolSize :: PoolSize
  } deriving (Eq, Show)
instance HasParser PersistPostgresqlConfig where
  parser = PersistPostgresqlConfig <$> parser <*> parser
instance HasPersistPostgresqlConfig PersistPostgresqlConfig where
  persistPostgresqlConfig = id

newtype ConnectionString = ConnectionString { unConnectionString :: ByteString }
  deriving newtype (Eq, IsString, Read, Show)
instance HasParser ConnectionString where
  parser
    = strOption
    $ long "ppcs"
    <> metavar "CONNECTION_STRING"
    <> help "Persist Postgresql Connection String e.g. \"host=localhost port=5432 user=test dbname=test password=test\""

newtype PoolSize = PoolSize { unPoolSize :: Int }
  deriving newtype (Eq, Read, Show)
instance HasParser PoolSize where
  parser
    = option auto
    $ long "ppps"
    <> metavar "POOL_SIZE"
    <> value (PoolSize 10)
    <> help "Persist Postgresql Pool Size"
