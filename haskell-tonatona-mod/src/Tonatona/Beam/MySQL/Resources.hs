module Tonatona.Beam.MySQL.Resources where

import RIO
import Database.MySQL.Base (ConnectInfo(..), defaultConnectInfo)

import Tonatona.WithResource (With, withResource, hoistWithResource)
import Tonatona.Beam.MySQL.Options (HasBeamMySQLOptions, beamMySQLOptionsL, _host, _port, _user, _password, _database)

class HasBeamMySQLResources resources where
  beamMySQLResourcesL :: Lens' resources BeamMySQLResources

-- class HasBeamMySQLConnection resources where
--   beamMySQLConnectionL :: Lens' resources Connection

-- Resource Initialization
data BeamMySQLResources = BeamMySQLResources
  { beamMySQLConnectInfo :: !BeamMySQLConnectInfo
  }
instance HasBeamMySQLResources BeamMySQLResources where
  beamMySQLResourcesL = id
instance HasBeamMySQLOptions options => With options BeamMySQLResources where
  withResource = BeamMySQLResources <$> withResource

newtype BeamMySQLConnectInfo
  = BeamMySQLConnectInfo { unBeamMySQLConnectInfo :: ConnectInfo }
_connectInfo :: Lens' BeamMySQLResources ConnectInfo
_connectInfo = lens (unBeamMySQLConnectInfo . beamMySQLConnectInfo) \x y -> x { beamMySQLConnectInfo = BeamMySQLConnectInfo y }
instance HasBeamMySQLOptions options => With options BeamMySQLConnectInfo where
  withResource = hoistWithResource \options cont -> do
    let connectInfo
          = defaultConnectInfo
            { connectHost = view (beamMySQLOptionsL._host) options
            , connectPort = view (beamMySQLOptionsL._port) options
            , connectUser = view (beamMySQLOptionsL._user) options
            , connectPassword = view (beamMySQLOptionsL._password) options
            , connectDatabase = view (beamMySQLOptionsL._database) options
            }
    cont . BeamMySQLConnectInfo $ connectInfo

-- newtype BeamMySQLConnection
--   = BeamMySQLConnection { beamMySQLConnection :: Connection }
-- instance HasBeamMySQLConnection BeamMySQLConnection where
--   beamMySQLConnectionL = lens beamMySQLConnection (\_ y -> BeamMySQLConnection y )

-- instance HasBeamMySQLOptions options => With options BeamMySQLConnection where
--   withResource = hoistWithResource \options cont -> do
--     let connectInfo
--           = defaultConnectInfo
--             { connectHost = view (beamMySQLOptionsL._host) options
--             , connectPort = view (beamMySQLOptionsL._port) options
--             , connectUser = view (beamMySQLOptionsL._user) options
--             , connectPassword = view (beamMySQLOptionsL._password) options
--             , connectDatabase = view (beamMySQLOptionsL._database) options
--             }
--     bracket (liftIO $ connect connectInfo) (liftIO . close)
--       $ cont . BeamMySQLConnection

