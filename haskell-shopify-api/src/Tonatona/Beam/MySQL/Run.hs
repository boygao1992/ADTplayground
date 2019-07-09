module Tonatona.Beam.MySQL.Run where

import RIO
import Database.Beam.MySQL (connect, close)
import Database.Beam.MySQL.Connection (MySQLM(..))
import qualified Database.Beam.MySQL.Connection as Beam (runBeamMySQL, runBeamMySQLDebug)
import Database.MySQL.Base (MySQLError)

import Tonatona.Beam.MySQL.Resources

runBeamMySQL
  :: HasBeamMySQLResources resources
  => MySQLM a
  -> RIO resources a
runBeamMySQL query = do
  connectInfo <- view (beamMySQLResourcesL._connectInfo)
  bracket (liftIO $ connect connectInfo) (liftIO . close) \connection ->
    liftIO $ Beam.runBeamMySQL connection query

runBeamMySQLSafe ::
  ( HasBeamMySQLResources resources
  , HasLogFunc resources
  )
  => MySQLM ()
  -> RIO resources ()
runBeamMySQLSafe query = runBeamMySQL query `catch` \(e :: MySQLError) -> do
  logError $ displayShow e

runBeamMySQLDebug ::
  ( HasBeamMySQLResources resources
  , HasLogFunc resources
  )
  => MySQLM a
  -> RIO resources a
runBeamMySQLDebug query = do
  connectInfo <- view (beamMySQLResourcesL._connectInfo)
  bracket (liftIO $ connect connectInfo) (liftIO . close) \connection ->
    withRunInIO \unlift ->
      Beam.runBeamMySQLDebug (unlift . logDebug . fromString) connection query

runBeamMySQLDebugSafe ::
  ( HasBeamMySQLResources resources
  , HasLogFunc resources
  )
  => MySQLM ()
  -> RIO resources ()
runBeamMySQLDebugSafe query =
  runBeamMySQLDebug query `catch` \(e :: MySQLError) -> do
    logError $ displayShow e
