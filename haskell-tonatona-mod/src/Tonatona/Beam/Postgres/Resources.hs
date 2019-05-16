module Tonatona.Beam.Postgres.Resources where

import RIO
import Database.Beam.Postgres (ConnectInfo(..), Connection, connect, close, defaultConnectInfo)

import Tonatona.WithResource (With, withResource, hoistWithResource)
import Tonatona.Beam.Postgres.Options

-- Resource Initialization

newtype BeamPostgresConnection
  = BeamPostgresConnection { beamPostgresConnection :: Connection }

instance HasBeamPostgresOptions options => With options BeamPostgresConnection where
  withResource = hoistWithResource \options cont -> do
    let connectInfo
          = defaultConnectInfo
            { connectHost = view (beamPostgresOptionsL._host) options
            , connectPort = view (beamPostgresOptionsL._port) options
            , connectUser = view (beamPostgresOptionsL._user) options
            , connectPassword = view (beamPostgresOptionsL._password) options
            , connectDatabase = view (beamPostgresOptionsL._database) options
            }
    bracket (liftIO $ connect connectInfo) (liftIO . close)
      $ cont . BeamPostgresConnection
