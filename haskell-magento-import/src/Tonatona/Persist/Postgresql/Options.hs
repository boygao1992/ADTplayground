module Tonatona.Persist.Postgresql.Options where

import RIO
import Options.Applicative (strOption, option, auto, long, metavar, value, help)
import Tonatona.Options.Parser (HasParser, parser)

class HasPersistPostgresqlOptions options where
  persistPostgresqlOptionsL :: Lens' options PersistPostgresqlOptions

data PersistPostgresqlOptions = PersistPostgresqlOptions
  { connectionString :: !ConnectionString
  , poolSize :: !PoolSize
  } deriving (Eq, Show)
instance HasParser PersistPostgresqlOptions where
  parser = PersistPostgresqlOptions <$> parser <*> parser
instance HasPersistPostgresqlOptions PersistPostgresqlOptions where
  persistPostgresqlOptionsL = id

newtype ConnectionString = ConnectionString { unConnectionString :: ByteString }
  deriving newtype (Eq, IsString, Read, Show)
_connectionString :: Lens' PersistPostgresqlOptions ByteString
_connectionString = lens (unConnectionString . connectionString) (\x y -> x { connectionString = ConnectionString y })
instance HasParser ConnectionString where
  parser
    = strOption
    $ long "ppcs"
    <> metavar "CONNECTION_STRING"
    <> help "set Persist Postgresql Connection String e.g. \"host=localhost port=5432 user=test dbname=test password=test\""

newtype PoolSize = PoolSize { unPoolSize :: Int }
  deriving newtype (Eq, Read, Show)
_poolSize :: Lens' PersistPostgresqlOptions Int
_poolSize = lens (unPoolSize . poolSize) (\x y -> x { poolSize = PoolSize y })
instance HasParser PoolSize where
  parser
    = option auto
    $ long "ppps"
    <> metavar "POOL_SIZE"
    <> value (PoolSize 10)
    <> help "set Persist Postgresql Pool Size"
