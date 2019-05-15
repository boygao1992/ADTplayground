module Tonatona.Beam.Postgres.Resources where

import RIO
import Database.Beam.Postgres (ConnectInfo(..), Connection, connect, defaultConnectInfo)

import Tonatona.WithResource (With, withResource, hoistWithResource)
import Tonatona.Beam.Postgres.Options

-- Resource Initialization

newtype BeamPostgresConnection
  = BeamPostgresConnection { beamPostgresConnection :: Connection }

instance HasBeamPostgresOptions options => With options BeamPostgresConnection where
  withResource = hoistWithResource $ \options cont -> do
    let connectInfo
          = defaultConnectInfo
            { connectHost = view (beamPostgresOptions._host) options
            , connectPort = view (beamPostgresOptions._port) options
            , connectUser = view (beamPostgresOptions._user) options
            , connectPassword = view (beamPostgresOptions._password) options
            , connectDatabase = view (beamPostgresOptions._database) options
            }
    liftIO (connect connectInfo) >>= (cont . BeamPostgresConnection)
