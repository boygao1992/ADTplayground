module Selda.Transform where

import Prelude

import Data.Array ((:))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (snd)

import Selda.Exp (Exp(..), SomeCol(..), allNamesIn, runSomeExp, some)
import Selda.Query.Type (GenState)
import Selda.SQL (SQL(..), SqlSource(..))
import Selda.Types (ColName)


-- | Remove all dead columns recursively, assuming that the given list of
--   column names contains all names present in the final result.
removeDeadCols :: Array ColName -> SQL -> SQL
removeDeadCols live sql = SQL
    case sql'.source of
      EmptyTable      -> sql'
      TableName _     -> sql'
      Values  _ _     -> sql'
      Product qs      -> sql' {source = Product $ map noDead qs}
      Join jt on l r  -> sql' {source = Join jt on (noDead l) (noDead r)}
  where
    noDead = removeDeadCols live'
    sql' = unwrap $ keepCols (allNonOutputColNames sql <> live) sql
    live' = allColNames (SQL sql')

-- | Return the names of all columns in the given top-level query.
--   Subqueries are not traversed.
allColNames :: SQL -> Array ColName
allColNames (SQL sql)
  = colNames sql.cols
  <> allNonOutputColNames (SQL sql)

-- | Return the names of all non-output (i.e. 'cols') columns in the given
--   top-level query. Subqueries are not traversed.
allNonOutputColNames :: SQL -> Array ColName
allNonOutputColNames (SQL sql) =
  Array.foldMap allNamesIn sql.restricts
  <> colNames sql.groups
  <> colNames (snd <$> sql.ordering)
  <> case sql.source of
    Join _ on _ _ -> allNamesIn on
    _ -> []

-- | Get all column names appearing in the given list of (possibly complex)
--   columns.
colNames :: Array (SomeCol SQL) -> Array ColName
colNames cs = cs >>= case _ of
  Some c -> runSomeExp c allNamesIn
  Named n c -> n : runSomeExp c allNamesIn

-- | Remove all columns but the given, named ones and aggregates, from a query's
--   list of outputs.
--   If we want to refer to a column in an outer query, it must have a name.
--   If it doesn't, then it's either not referred to by an outer query, or
--   the outer query duplicates the expression, thereby referring directly
--   to the names of its components.
keepCols :: Array ColName -> SQL -> SQL
keepCols live (SQL sql) = SQL $ sql { cols = filtered }
  where
    filtered = Array.filter (_ `oneOf` live) sql.cols
    oneOf (Some someExp) ns = runSomeExp someExp case _ of
      AggrEx _ _ -> true
      Col n -> Array.elem n ns
      _ -> false
    oneOf (Named n someExp) ns = runSomeExp someExp case _ of
      AggrEx _ _ -> true
      _ -> Array.elem n ns

-- | Build the outermost query from the SQL generation state.
--   Groups are ignored, as they are only used by 'aggregate'.
state2sql :: GenState -> SQL
state2sql { sources, staticRestricts } = case sources of
  [(SQL sql)] -> SQL $ sql { restricts = sql.restricts <> staticRestricts }
  ss -> SQL
    { cols: allCols ss
    , source: Product ss
    , restricts: staticRestricts
    , groups: []
    , ordering: []
    , limits: Nothing
    , distinct: false
    }

-- | Get all output columns from a list of SQL ASTs.
allCols :: Array SQL -> Array (SomeCol SQL)
allCols sqls = do
  (SQL sql) <- sqls
  col <- sql.cols
  pure $ case col of
    Named n _ -> some $ Col n
    Some c -> Some c

