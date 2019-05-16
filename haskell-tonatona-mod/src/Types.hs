module Types where

import RIO
-- import RIO.Process

import Tonatona.Options.Parser
import Tonatona.Logger.Options
import Tonatona.Beam.MySQL.Options

import Tonatona.WithResource
import Tonatona.Logger.Resources
import Tonatona.Beam.MySQL.Resources

data Options = Options
  { optionsLogger :: !LoggerOptions
  , optionsBeamMySQL :: !BeamMySQLOptions
  }
instance HasParser Options where
  parser = Options <$> parser <*> parser
instance HasLoggerOptions Options where
  loggerOptionsL = lens optionsLogger (\x y -> x { optionsLogger = y })
instance HasBeamMySQLOptions Options where
  beamMySQLOptionsL = lens optionsBeamMySQL (\x y -> x { optionsBeamMySQL = y })

data Resources = Resources
  { resourceLogFunc :: !LoggerLogFunc
  , resourceBeamMySQLConnection :: !BeamMySQLConnection
  }
instance With Options Resources where
  withResource = Resources <$> withResource <*> withResource
instance HasLogFunc Resources where
  logFuncL = lens resourceLogFunc (\x y -> x { resourceLogFunc = y }) . logFuncL
instance HasBeamMySQLConnection Resources where
  beamMySQLConnectionL = lens resourceBeamMySQLConnection (\x y -> x { resourceBeamMySQLConnection = y }) . beamMySQLConnectionL

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
