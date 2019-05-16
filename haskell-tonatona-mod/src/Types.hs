module Types where

import RIO
-- import RIO.Process

import Tonatona.Options.Parser
import Tonatona.Logger.Options
import Tonatona.Beam.Postgres.Options

import Tonatona.WithResource
import Tonatona.Logger.Resources
import Tonatona.Beam.Postgres.Resources

data Options = Options
  { optionsLogger :: !LoggerOptions
  , optionsBeamPostgresOptions :: !BeamPostgresOptions
  }
instance HasParser Options where
  parser = Options <$> parser <*> parser
instance HasLoggerOptions Options where
  loggerOptionsL = lens optionsLogger (\x y -> x { optionsLogger = y })
instance HasBeamPostgresOptions Options where
  beamPostgresOptionsL = lens optionsBeamPostgresOptions (\x y -> x { optionsBeamPostgresOptions = y })

data Resources = Resources
  { resourceLogFunc :: !LoggerLogFunc
  , resourceBeamPostgresConnection :: !BeamPostgresConnection
  }
instance With Options Resources where
  withResource = Resources <$> withResource <*> withResource
instance HasLogFunc Resources where
  logFuncL = lens resourceLogFunc (\x y -> x { resourceLogFunc = y }) . logFuncL


-- data AppOptions = AppOptions
--   { appLoggerOptions :: LoggerOptions
--   , appBeamPostgresqlOptions :: BeamPostgresqlOptions
--   }
-- instance HasLoggerOptions AppOptions where
--   loggerOptions = lens appLoggerOptions (\x y -> x { appLoggerOptions = y })
-- instance HasBeamPostgresqlOptions AppOptions where
--   beamPostgresqlOptions = lens appBeamPostgresqlOptions (\x y -> x { appBeamPostgresqlOptions = y })

-- data Config = Config
--   { logFunc :: LogFunc
--   , beamPostgresqlConnection :: Connection
--   }

-- instance With AppOptions Config where
--   withResource = Config <$> withResource <*> withResource

-- -- | Command line arguments
-- data Options = Options
--   { optionsVerbose :: !Bool
--   }

-- data App = App
--   { appLogFunc :: !LogFunc
--   , appProcessContext :: !ProcessContext
--   , appOptions :: !Options
--   -- Add other app-specific configuration information here
--   }

-- instance HasLogFunc App where
--   logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
-- instance HasProcessContext App where
--   processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })
