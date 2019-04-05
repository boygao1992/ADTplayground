module MySQL
( module Exports
, createConnection
, closeConnection
, queryWithOptions
, queryWithOptions_
, query
, query_
, execute
, execute_
) where

import MySQL.Connection
  ( ConnectionInfo
  , QueryOptions
  , Connection
  , defaultConnectionInfo
  , format
  ) as Exports

import MySQL.Pool
  ( PoolInfo
  , Pool
  , defaultPoolInfo
  , createPool
  , closePool
  , withPool
  ) as Exports

import MySQL.QueryValue
  ( QueryValue
  , class Queryable
  , toQueryValue
  ) as Exports

import Prelude

import MySQL.Connection as SQL
import MySQL.QueryValue as SQL
import Run (Run, AFF, EFFECT)
import Run as Run
import Run.Reader (READER)
import Run.Reader (ask) as Reader
import Simple.JSON (class ReadForeign)

createConnection
  :: forall eff
   . SQL.ConnectionInfo
  -> Run
      ( effect :: EFFECT
      | eff
      )
      SQL.Connection
createConnection = Run.liftEffect <<< SQL.createConnection

closeConnection
  :: forall eff
   . SQL.Connection
  -> Run
      ( effect :: EFFECT
      | eff
      )
      Unit
closeConnection = Run.liftEffect <<< SQL.closeConnection

queryWithOptions
  :: forall a eff
   . ReadForeign a
  => SQL.QueryOptions
  -> Array SQL.QueryValue
  -> Run
      ( reader :: READER SQL.Connection
      , aff :: AFF
      | eff
      )
      (Array a)
queryWithOptions opts vs = do
  conn <- Reader.ask
  Run.liftAff $ SQL.queryWithOptions opts vs conn

queryWithOptions_
  :: forall a eff
   . ReadForeign a
  => SQL.QueryOptions
  -> Run
      ( reader :: READER SQL.Connection
      , aff :: AFF
      | eff
      )
      (Array a)
queryWithOptions_ opts = queryWithOptions opts []

query
  :: forall a eff
   . ReadForeign a
  => String
  -> Array SQL.QueryValue
  -> Run
      ( reader :: READER SQL.Connection
      , aff :: AFF
      | eff
      )
      (Array a)
query sql = queryWithOptions { sql, nestTables: false }


query_
  :: forall a eff
   . ReadForeign a
  => String
  -> Run
       ( reader :: READER SQL.Connection
       , aff :: AFF
       | eff
       )
       (Array a)
query_ sql = query sql []

execute
  :: forall eff
   . String
  -> Array SQL.QueryValue
  -> Run
      ( reader :: READER SQL.Connection
      , aff :: AFF
      | eff
      )
      Unit
execute sql vs = do
  conn <- Reader.ask
  Run.liftAff $ SQL.execute sql vs conn

execute_
  :: forall eff
     . String
    -> Run
      ( reader :: READER SQL.Connection
      , aff :: AFF
      | eff
      )
      Unit
execute_ sql = execute sql []
