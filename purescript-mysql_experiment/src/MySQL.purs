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
, createPool
, closePool
, withPool
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
  ) as Exports

import MySQL.QueryValue
  ( QueryValue
  , class Queryable
  , toQueryValue
  ) as Exports

import Prelude

import Effect.Aff (Aff)
import MySQL.Connection (Connection, ConnectionInfo, QueryOptions, closeConnection, createConnection, execute, queryWithOptions) as SQL
import MySQL.Pool (Pool, PoolInfo, closePool, createPool, withPool) as SQL
import MySQL.QueryValue (QueryValue) as SQL
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

createPool
  :: forall eff
   . SQL.ConnectionInfo
  -> SQL.PoolInfo
  -> Run
      ( effect :: EFFECT
      | eff
      )
      SQL.Pool
createPool = (Run.liftEffect <<< _) <<< SQL.createPool

closePool
  :: forall eff
   . SQL.Pool
   -> Run
      ( effect :: EFFECT
      | eff
      )
      Unit
closePool = Run.liftEffect <<< SQL.closePool

withPool
  :: forall a eff
   . (SQL.Connection -> Aff a)
  -> SQL.Pool
  -> Run
      ( aff :: AFF
      | eff
      )
      a
withPool = (Run.liftAff <<< _) <<< SQL.withPool
