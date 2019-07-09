module MySQL.Connection
  ( ConnectionInfo
  , QueryOptions
  , Connection
  , defaultConnectionInfo
  , queryWithOptions
  , queryWithOptions_
  , query
  , query_
  , execute
  , execute_
  , format
  , createConnection
  , closeConnection
  , _query

  , queryWithOptions'
  , queryWithOptions'_
  , query'
  , query'_
  ) where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either, either)
import Data.Function.Uncurried (Fn3, runFn3)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Foreign (Foreign)
import MySQL.Internal (liftError)
import MySQL.QueryValue (QueryValue)
import Simple.JSON (class ReadForeign, read)

type ConnectionInfo =
    { host :: String
    , port :: Int
    , user :: String
    , password :: String
    , database :: String
    , charset :: String
    , timezone :: String
    , connectTimeout :: Number
    , dateStrings :: Boolean
    , debug :: Boolean
    , trace :: Boolean
    , multipleStatements :: Boolean
    }

type QueryOptions =
  { sql :: String
  , nestTables :: Boolean
  , timeout :: Number
  }

foreign import data Connection :: Type



defaultConnectionInfo :: ConnectionInfo
defaultConnectionInfo =
  { host: "localhost"
  , port: 3306
  , user: "root"
  , password: ""
  , database: ""
  , charset: "UTF8_GENERAL_CI"
  , timezone: "Z"
  , connectTimeout: 100000.0
  , dateStrings: true
  , debug: false
  , trace: true
  , multipleStatements : false
  }



queryWithOptions
  :: forall a
   . ReadForeign a
  => QueryOptions
  -> Array QueryValue
  -> Connection
  -> Aff (Array a)
queryWithOptions opts vs conn = do
  rows <- _query opts vs conn
  either liftError pure  $ read rows

queryWithOptions'
  :: forall a
   . ReadForeign a
  => QueryOptions
  -> Array QueryValue
  -> Connection
  -> Aff (Either String (Array a))
queryWithOptions' opts vs conn = do
  rows <- _query opts vs conn
  pure <<< lmap show $ read rows

queryWithOptions_
  :: forall a
   . ReadForeign a
  => QueryOptions
  -> Connection
  -> Aff (Array a)
queryWithOptions_ opts = queryWithOptions opts []


queryWithOptions'_
  :: forall a
   . ReadForeign a
  => QueryOptions
  -> Connection
  -> Aff (Either String (Array a))
queryWithOptions'_ opts = queryWithOptions' opts []

query
  :: forall a
   . ReadForeign a
  => String
  -> Array QueryValue
  -> Connection
  -> Aff (Array a)
query sql = queryWithOptions { sql, nestTables: false , timeout: 3600000.0 }

query'
  :: forall a
   . ReadForeign a
  => String
  -> Array QueryValue
  -> Connection
  -> Aff (Either String (Array a))
query' sql = queryWithOptions' { sql, nestTables: false , timeout: 3600000.0 }



query_
  :: forall a
   . ReadForeign a
  => String
  -> Connection
  -> Aff (Array a)
query_ sql = query sql []

query'_
  :: forall a
   . ReadForeign a
  => String
  -> Connection
  -> Aff (Either String (Array a))
query'_ sql = query' sql []



execute
  :: String
  -> Array QueryValue
  -> Connection
  -> Aff Unit
execute sql vs conn =
  void $ _query { sql, nestTables: false, timeout: 3600000.0 } vs conn

execute_
  :: String
  -> Connection
  -> Aff Unit
execute_ sql = execute sql []



_query
  :: QueryOptions
  -> Array QueryValue
  -> Connection
  -> Aff Foreign
_query opts values conn = fromEffectFnAff $ runFn3 _query' opts values conn

foreign import createConnection
  :: ConnectionInfo
  -> Effect Connection



foreign import closeConnection
  :: Connection
  -> Effect Unit



foreign import _query'
  :: Fn3 QueryOptions (Array QueryValue) Connection (EffectFnAff Foreign)



foreign import format :: String -> (Array QueryValue) -> Connection -> String
