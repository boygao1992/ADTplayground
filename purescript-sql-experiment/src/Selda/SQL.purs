module Selda.SQL where

import Prelude

import Data.Exists (Exists)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Selda.Exp (Exp, SomeCol)
import Selda.SqlType (Lit)
import Selda.Types (TableName)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

-- | A source for an SQL query.
data SqlSource
  = TableName TableName
  | Product (Array SQL)
  | Join JoinType (Exp SQL Boolean) SQL SQL
  | Values (Array (SomeCol SQL)) (Array (Array Param))
  | EmptyTable

-- | Type of join to perform.
data JoinType = InnerJoin | LeftJoin

-- | AST for SQL queries.
newtype SQL = SQL
  { cols      :: Array (SomeCol SQL)
  , source    :: SqlSource
  , restricts :: Array (Exp SQL Boolean)
  , groups    :: Array (SomeCol SQL)
  , ordering  :: Array (Order /\ SomeCol SQL)
  , limits    :: Maybe (Int /\ Int)
  , distinct  :: Boolean
  }

-- TODO instance Names SqlSource where
-- TODO instance Names SQL where

-- TODO sqlFrom :: [SomeCol SQL] -> SqlSource -> SQL

-- | The order in which to sort result rows.
data Order = Asc | Desc
derive instance eqOrder :: Eq Order
derive instance ordOrder :: Ord Order
derive instance genericOrder :: Generic Order _
instance showOrder :: Show Order where show = genericShow

-- | A parameter to a prepared SQL statement.
newtype Param = Param (Exists Lit)
-- TODO instance Eq, Ord, Show

-- | Create a parameter from the given value.
-- TODO param :: SqlType a => a -> Param

-- | The SQL type of the given parameter.
-- TODO paramType :: Param -> SqlTypeRep
