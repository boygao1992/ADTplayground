module Types where

import RIO
-- import RIO.Process

import Tonatona.Options.Parser
import Tonatona.Logger.Options
import Tonatona.Beam.MySQL.Options
import Tonatona.Servant.Options

import Tonatona.WithResource
import Tonatona.Logger.Resources
import Tonatona.Beam.MySQL.Resources
import Tonatona.Servant.Resources

data Options = Options
  { optionsLogger :: !LoggerOptions
  , optionsBeamMySQL :: !BeamMySQLOptions
  , optionsServant :: !ServantOptions
  }
instance HasParser Options where
  parser = Options <$> parser <*> parser <*> parser
instance HasLoggerOptions Options where
  loggerOptionsL = lens optionsLogger (\x y -> x { optionsLogger = y })
instance HasBeamMySQLOptions Options where
  beamMySQLOptionsL = lens optionsBeamMySQL (\x y -> x { optionsBeamMySQL = y })
instance HasServantOptions Options where
  servantOptionsL = lens optionsServant (\x y -> x { optionsServant = y })

data Resources = Resources
  { resourceLogFunc :: !LoggerLogFunc
  , resourceBeamMySQLConnection :: !BeamMySQLConnection
  , resourceServant :: !ServantResources
  }
instance With Options Resources where
  withResource = Resources <$> withResource <*> withResource <*> withResource
instance HasLogFunc Resources where
  logFuncL = lens resourceLogFunc (\x y -> x { resourceLogFunc = y }) . logFuncL
instance HasBeamMySQLConnection Resources where
  beamMySQLConnectionL = lens resourceBeamMySQLConnection (\x y -> x { resourceBeamMySQLConnection = y }) . beamMySQLConnectionL
instance HasServantResources Resources where
  servantResourcesL = lens resourceServant (\x y -> x { resourceServant = y })
