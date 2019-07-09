module Tonatona.Beam.Postgres.Run where

import RIO
import qualified Database.Beam.Postgres as Postgres (connect, close, runBeamPostgres, runBeamPostgresDebug)
import Database.Beam.Postgres (Pg, SqlError)

import Tonatona.Beam.Postgres.Resources

runBeamPostgres
  :: HasBeamPostgresResources resources
  => Pg a
  -> RIO resources a
runBeamPostgres query = do
  connectInfo <- view (beamPostgresResourcesL._connectInfo)
  bracket (liftIO $ Postgres.connect connectInfo) (liftIO . Postgres.close) \connection ->
    liftIO $ Postgres.runBeamPostgres connection query

runBeamPostgresSafe ::
  ( HasBeamPostgresResources resources
  , HasLogFunc resources
  )
  => Pg ()
  -> RIO resources ()
runBeamPostgresSafe query = runBeamPostgres query `catch` \(e :: SqlError) -> do
  logError $ displayShow e

runBeamPostgresDebug ::
  ( HasBeamPostgresResources resources
  , HasLogFunc resources
  )
  => Pg a
  -> RIO resources a
runBeamPostgresDebug query = do
  connectInfo <- view (beamPostgresResourcesL._connectInfo)
  bracket (liftIO $ Postgres.connect connectInfo) (liftIO . Postgres.close) \connection ->
    withRunInIO \unlift ->
      Postgres.runBeamPostgresDebug (unlift . logDebug . fromString) connection query

runBeamPostgresDebugSafe ::
  ( HasBeamPostgresResources resources
  , HasLogFunc resources
  )
  => Pg ()
  -> RIO resources ()
runBeamPostgresDebugSafe query =
  runBeamPostgresDebug query `catch` \(e :: SqlError) -> do
    logError $ displayShow e
