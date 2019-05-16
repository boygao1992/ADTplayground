module Tonatona.Beam.MySQL.Run where

import RIO
import Database.Beam.MySQL.Connection (MySQLM(..))
import qualified Database.Beam.MySQL.Connection as Beam (runBeamMySQL, runBeamMySQLDebug)

import Tonatona.Beam.MySQL.Resources

runBeamMySQL
  :: HasBeamMySQLConnection resources
  => MySQLM a
  -> RIO resources a
runBeamMySQL query = do
  connection <- view beamMySQLConnectionL
  liftIO $ Beam.runBeamMySQL connection query

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
