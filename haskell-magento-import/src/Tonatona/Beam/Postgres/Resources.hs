module Tonatona.Beam.Postgres.Resources where

import RIO
import Database.Beam.Postgres (ConnectInfo(..), defaultConnectInfo)

import Tonatona.WithResource (With, withResource, hoistWithResource)
import Tonatona.Beam.Postgres.Options (HasBeamPostgresOptions, beamPostgresOptionsL, _host, _port, _user, _password, _database)

class HasBeamPostgresResources resources where
  beamPostgresResourcesL :: Lens' resources BeamPostgresResources

data BeamPostgresResources = BeamPostgresResources
  { beamPostgresConnectInfo :: !BeamPostgresConnectInfo
  }
instance HasBeamPostgresResources BeamPostgresResources where
  beamPostgresResourcesL = id
instance HasBeamPostgresOptions options => With options BeamPostgresResources where
  withResource = BeamPostgresResources <$> withResource

-- Resource Initialization

newtype BeamPostgresConnectInfo
  = BeamPostgresConnectInfo { unBeamPostgresConnectInfo :: ConnectInfo }
_connectInfo :: Lens' BeamPostgresResources ConnectInfo
_connectInfo = lens (unBeamPostgresConnectInfo . beamPostgresConnectInfo) \x y -> x { beamPostgresConnectInfo = BeamPostgresConnectInfo y }
instance HasBeamPostgresOptions options => With options BeamPostgresConnectInfo where
  withResource = hoistWithResource \options cont -> do
    let connectInfo
          = defaultConnectInfo
            { connectHost = view (beamPostgresOptionsL._host) options
            , connectPort = view (beamPostgresOptionsL._port) options
            , connectUser = view (beamPostgresOptionsL._user) options
            , connectPassword = view (beamPostgresOptionsL._password) options
            , connectDatabase = view (beamPostgresOptionsL._database) options
            }
    cont . BeamPostgresConnectInfo $ connectInfo
