module Selda.Query where

import Prelude
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

import Control.Monad.State (get, modify_, put)
import Data.Array ((:))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Exists1 (runExists)
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (over)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))

import Selda.Column (class Columns, class Same, Col(..), Row(..), fromTup, toTup)
import Selda.Exp (Exp(..), NulOp(..), SomeCol(..), col, hideRenaming, lit, named, some)
import Selda.Generic (class GRelation, gNew, params)
import Selda.Inner (class AggrCols, class Aggregates, class LeftCols, class OuterCols, Aggr(..), Inner, unAggrs)
import Selda.Query.Type (Query(..), freshName, isolate, renameAll)
import Selda.SQL (JoinType(..), Order(..), Param(..), SQL(..), SqlSource(..), sqlFrom)
import Selda.SqlType (class SqlType)
import Selda.Table (Table(..))
import Selda.Transform (allCols, colNames, state2sql)


-- | Query the given table.
select :: forall s a. Table a -> Query s (Row s a)
select (Table {tableName: name, tableCols: cs}) = Query $ do
  rns <- renameAll $ _.colExpr <$> cs
  st <- get
  put $ st { sources = sqlFrom rns (TableName name) : st.sources }
  pure $ Many (map hideRenaming rns)

-- | Query an ad hoc table of type @a@. Each element in the given list represents
--   one row in the ad hoc table.
selectValues
  :: forall s a
  . GRelation a
  => Array a -> Query s (Row s a)
selectValues = Array.uncons >>> case _ of
  Nothing -> Query $ do
    st <- get
    put $ st { sources = sqlFrom [] EmptyTable : st.sources }
    pure $ Many $ gNew (Proxy :: Proxy a)
  Just { head: row, tail: rows } -> Query do
    names <- traverse (const freshName) firstrow
    let rns = (\n -> named n (col n)) <$> names
        row' = mkFirstRow names
        rows' = map (map defToVal <<< params) rows
    s <- get
    put $ s { sources = sqlFrom rns (Values row' rows') : s.sources }
    pure $ Many $ map hideRenaming rns

    where
      firstrow = map defToVal $ params row

      mkFirstRow ns = do
      -- [ Named n (Lit l) | (Param l, n) <- zip firstrow ns ]
        (Param l /\ n) <- Array.zip firstrow ns
        pure $ runExists (named n <<< lit) l

      defToVal (Left x)  = x
      defToVal (Right x) = x

-- | Restrict the query somehow. Roughly equivalent to @WHERE@.
restrict :: forall s t. Same s t => Col s Boolean -> Query t Unit
restrict (One p) = Query do
  st <- get
  put $ case st.sources of
    [] ->
      st { staticRestricts = p : st.staticRestricts }
    -- PostgreSQL doesn't put renamed columns in scope in the WHERE clause
    -- of the query where they are renamed, so if the restrict predicate
    -- contains any vars renamed in this query, we must add another query
    -- just for the restrict.
    [SQL sql] | not $ p `wasRenamedIn` sql.cols ->
      st { sources = [ SQL $ sql {restricts = p : sql.restricts }]}
    ss ->
      st { sources = [ over SQL (_ {restricts = [p]})
                        $ sqlFrom (allCols ss) (Product ss)]}
  where
    wasRenamedIn predicate cs =
      let cs' = Array.mapMaybe <@> cs $ case _ of
            Named n _ -> Just n
            _ -> Nothing
      in Array.any (_ `Array.elem` cs') (colNames [some predicate])

-- | Execute a query, returning an aggregation of its results.
--   The query must return an inductive tuple of 'Aggregate' columns.
--   When @aggregate@ returns, those columns are converted into non-aggregate
--   columns, which may then be used to further restrict the query.
--
--   Note that aggregate queries must not depend on outer queries, nor must
--   they return any non-aggregate columns. Attempting to do either results in
--   a type error.
--
--   The SQL @HAVING@ keyword can be implemented by combining @aggregate@
--   and 'restrict':
--
-- > -- Find the number of people living on every address, for all addresses
-- > -- with more than one tenant:
-- > -- SELECT COUNT(name) AS c, address FROM housing GROUP BY name HAVING c > 1
-- >
-- > numPpl = do
-- >   (num_tenants :*: theAddress) <- aggregate $ do
-- >     h <- select housing
-- >     theAddress <- groupBy (h ! address)
-- >     return (count (h ! address) :*: theAddress)
-- >  restrict (num_tenants .> 1)
-- >  return (num_tenants :*: theAddress)
aggregate
  :: forall s a o
  . AggrCols a o
  => Columns o
  => Aggregates a
  => Query (Inner s) a
  -> Query s o
aggregate q = Query $ do
  (gst /\ aggrs) <- isolate q
  cs <- renameAll $ unAggrs aggrs
  let sql = over SQL (_ { groups = gst.groupCols })
            $ sqlFrom cs (Product [state2sql gst])
  modify_ \st -> st { sources = sql : st.sources }
  pure $ toTup $ Array.mapMaybe <@> cs $ case _ of
    Named n _ -> Just n
    _ -> Nothing


-- | Perform a @LEFT JOIN@ with the current result set (i.e. the outer query)
--   as the left hand side, and the given query as the right hand side.
--   Like with 'aggregate', the inner (or right) query must not depend on the
--   outer (or right) one.
--
--   The given predicate over the values returned by the inner query determines
--   for each row whether to join or not. This predicate may depend on any
--   values from the outer query.
--
--   For instance, the following will list everyone in the @people@ table
--   together with their address if they have one; if they don't, the address
--   field will be @NULL@.
--
-- > getAddresses :: Query s (Col s Text :*: Col s (Maybe Text))
-- > getAddresses = do
-- >   (name :*: _) <- select people
-- >   (_ :*: address) <- leftJoin (\(n :*: _) -> n .== name)
-- >                               (select addresses)
-- >   return (name :*: address)
leftJoin
  :: forall s a oc lc
  . OuterCols a oc
  => LeftCols a lc
  => Columns a
  => Columns oc
  => Columns lc
  => (oc -> Col s Boolean)
  -- ^ Predicate determining which lines to join.
  -- | Right-hand query to join.
  -> Query (Inner s) a
  -> Query s lc
leftJoin = someJoin LeftJoin

-- | Perform an @INNER JOIN@ with the current result set and the given query.
innerJoin
  :: forall s a oc
  . OuterCols a oc
  => Columns a
  => Columns oc
  => (oc -> Col s Boolean)
  -- ^ Predicate determining which lines to join.
  -- | Right-hand query to join.
  -> Query (Inner s) a
  -> Query s oc
innerJoin = someJoin InnerJoin

-- | The actual code for any join.
someJoin
  :: forall s a a' oc
  . OuterCols a oc
  => Columns a
  => Columns oc
  => Columns a'
  => JoinType
  -> (oc -> Col s Boolean)
  -> Query (Inner s) a
  -> Query s a'
someJoin jointype check q = Query $ do
  (join_st /\ res) <- isolate q
  cs <- renameAll $ fromTup res
  st <- get
  let
    nameds  = Array.mapMaybe <@> cs $ case _ of
      Named n _ -> Just n
      _ -> Nothing
    left    = state2sql st
    right   = sqlFrom cs (Product [state2sql join_st])
    One on  = check $ toTup nameds
    outCols = ((some <<< col) <$> nameds) <> allCols [left]
  put $ st {sources = [sqlFrom outCols (Join jointype on left right)]}
  pure $ toTup nameds

-- | Group an aggregate query by a column.
--   Attempting to group a non-aggregate query is a type error.
--   An aggregate representing the grouped-by column is returned, which can be
--   returned from the aggregate query. For instance, if you want to find out
--   how many people have a pet at home:
--
-- > aggregate $ do
-- >   person <- select people
-- >   name' <- groupBy (person ! name)
-- >   return (name' :*: count(person ! pet_name) .> 0)
groupBy
  :: forall s t a
  . Same s t
  => SqlType a
  => Col (Inner s) a
  -> Query (Inner t) (Aggr (Inner t) a)
groupBy (One c) = Query $ do
  st <- get
  put $ st {groupCols = some c : st.groupCols }
  pure (Aggr c)


-- | Drop the first @m@ rows, then get at most @n@ of the remaining rows from the
--   given subquery.
limit
  :: forall s t a oc
  . Same s t
  => OuterCols a oc
  => Int
  -> Int
  -> Query (Inner s) a
  -> Query t oc
limit from to q = Query $ do
  (lim_st /\ res) <- isolate q
  st <- get
  let
    sql' = case lim_st.sources of
      [SQL sql] | isNothing sql.limits -> SQL sql
      ss -> sqlFrom (allCols ss) (Product ss)
  put $ st {sources = over SQL (_ {limits = Just (from /\ to)}) sql' : st.sources }
  -- TODO: replace with safe coercion
  pure $ unsafeCoerce res

-- | Sort the result rows in ascending or descending order on the given row.
--
--   If multiple @order@ directives are given, later directives are given
--   precedence but do not cancel out earlier ordering directives.
--   To get a list of persons sorted primarily on age and secondarily on name:
--
-- > peopleInAgeAndNameOrder = do
-- >   person <- select people
-- >   order (person ! name) ascending
-- >   order (person ! age) ascending
-- >   return (person ! name)
--
--   For a table @[("Alice", 20), ("Bob", 20), ("Eve", 18)]@, this query
--   will always return @["Eve", "Alice", "Bob"]@.
--
--   The reason for later orderings taking precedence and not the other way
--   around is composability: @order@ should always sort the current
--   result set to avoid weird surprises when a previous @order@ directive
--   is buried somewhere deep in an earlier query.
--   However, the ordering must always be stable, to ensure that previous
--   calls to order are not simply erased.
order
  :: forall s t a
  . Same s t
  => SqlType a
  => Col s a
  -> Order
  -> Query t Unit
order (One c) o = Query do
  st <- get
  case st.sources of
    [SQL sql] -> put st {sources = [SQL sql {ordering = (o /\ some c) : sql.ordering }]}
    ss -> put st {sources = [over SQL (_ {ordering = [(o /\ some c)]}) sql ]}
      where sql = sqlFrom (allCols ss) (Product ss)

-- | Sort the result rows in random order.
orderRandom :: forall s. Query s Unit
orderRandom = order (One (NulOp (Fun0 "RANDOM") :: Exp SQL Int) :: Col s Int) Asc

-- | Remove all duplicates from the result set.
distinct
  :: forall s a oc
  . OuterCols a oc
  => Columns a
  => Columns oc
  => Query (Inner s) a
  -> Query s oc
distinct q = Query $ do
  (inner_st /\ res) <- isolate q
  st <- get
  let ss = inner_st.sources
  put st { sources = [ over SQL (_ { distinct = true })
                       $ sqlFrom (allCols ss) (Product ss)]}
  -- TODO: replace with safe coercion
  pure $ unsafeCoerce res
