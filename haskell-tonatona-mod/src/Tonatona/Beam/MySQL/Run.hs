module Tonatona.Beam.MySQL.Run where

import RIO
import Database.Beam.MySQL.Connection (MySQLM(..))
import qualified Database.Beam.MySQL.Connection as Beam (runBeamMySQL, runBeamMySQLDebug)
import Database.MySQL.Base (MySQLError)

import Tonatona.Beam.MySQL.Resources

runBeamMySQL
  :: HasBeamMySQLConnection resources
  => MySQLM a
  -> RIO resources a
runBeamMySQL query = do
  connection <- view beamMySQLConnectionL
  liftIO $ Beam.runBeamMySQL connection query

runBeamMySQLSafe ::
  ( HasBeamMySQLConnection resources
  , HasLogFunc resources
  )
  => MySQLM ()
  -> RIO resources ()
runBeamMySQLSafe query = runBeamMySQL query `catch` \(e :: MySQLError) -> do
  logError $ displayShow e

runBeamMySQLDebug ::
  ( HasBeamMySQLConnection resources
  , HasLogFunc resources
  )
  => MySQLM a
  -> RIO resources a
runBeamMySQLDebug query = do
  connection <- view beamMySQLConnectionL
  withRunInIO \unlift ->
    Beam.runBeamMySQLDebug (unlift . logDebug . fromString) connection query

runBeamMySQLDebugSafe ::
  ( HasBeamMySQLConnection resources
  , HasLogFunc resources
  )
  => MySQLM ()
  -> RIO resources ()
runBeamMySQLDebugSafe query =
  runBeamMySQLDebug query `catch` \(e :: MySQLError) -> do
    logError $ displayShow e
