module Selda.SQL where

import Prelude

import Data.Array as Array
import Data.Exists (Exists, mkExists, runExists)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\))
import Selda.Exp (class Names, Exp, SomeCol, allNamesIn)
import Selda.SqlType (class SqlType, Lit, mkLit)
import Selda.Types (TableName)

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
derive instance newtypeSQL :: Newtype SQL _

instance namesSqlSource :: Names SqlSource where
  allNamesIn (Product qs)   = Array.foldMap allNamesIn qs
  allNamesIn (Join _ e l r) = allNamesIn e <> allNamesIn l <> allNamesIn r
  allNamesIn (Values vs _)  = allNamesIn vs
  allNamesIn (TableName _)  = []
  allNamesIn (EmptyTable)   = []

instance namesSQL :: Names SQL where
  -- Note that we don't include @cols@ here: the names in @cols@ are not
  -- necessarily used, only declared.
  allNamesIn (SQL { groups, ordering, restricts, source })
    = allNamesIn groups
    <> Array.foldMap (allNamesIn <<< snd) ordering
    <> allNamesIn restricts
    <> allNamesIn source


-- | Build a plain SQL query with the given columns and source, with no filters,
--   ordering, etc.
sqlFrom :: Array (SomeCol SQL) -> SqlSource -> SQL
sqlFrom cs src = SQL
  { cols: cs
  , source: src
  , restricts: []
  , groups: []
  , ordering: []
  , limits: Nothing
  , distinct: false
  }

-- | The order in which to sort result rows.
data Order = Asc | Desc
derive instance eqOrder :: Eq Order
derive instance ordOrder :: Ord Order
derive instance genericOrder :: Generic Order _
instance showOrder :: Show Order where show = genericShow

-- | A parameter to a prepared SQL statement.
newtype Param = Param (Exists Lit)
-- TODO instance Eq, Ord, Show
mkParam :: forall a. Lit a -> Param
mkParam = Param <<< mkExists
runParam :: forall r. (forall a. Lit a -> r) -> Param -> r
runParam f (Param p) = runExists f p

-- | Create a parameter from the given value.
param :: forall a. SqlType a => a -> Param
param = mkParam <<< mkLit

-- | The SQL type of the given parameter.
-- TODO paramType :: Param -> SqlTypeRep
