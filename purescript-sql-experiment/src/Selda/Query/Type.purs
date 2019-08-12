module Selda.Query.Type where

import Prelude

import Control.Monad.State (State, get, put, runState)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Selda.Exp (Exp(..), SomeCol(..), UntypedCol(..), runSomeExp)
import Selda.SQL (SQL)
import Selda.Types

type Scope = Int
type Ident = Int

-- | A name, consisting of a scope and an identifier.
data Name = Name Scope Ident

instance showName :: Show Name where
  show (Name 0 n) = show n
  show (Name s n) = show s <> "s_" <> show n

  -- | An SQL query.
newtype Query s a = Query (State GenState a)
derive newtype instance functorQuery :: Functor (Query s)
derive newtype instance applyQuery :: Apply (Query s)
derive newtype instance applicativeQuery :: Applicative (Query s)
derive newtype instance bindQuery :: Bind (Query s)
derive newtype instance monadQuery :: Monad (Query s)

-- | Run a query computation from an initial state.
runQueryM :: forall s a. Scope -> Query s a -> a /\ GenState
runQueryM scope (Query q) = flip runState (initState scope) q

-- | Run a query computation in isolation, but reusing the current name supply.
isolate :: forall s a. Query s a -> State GenState (GenState /\ a)
isolate (Query q) = do
  st <- get
  put $ (initState (st.nameScope)) { nameSupply = st.nameSupply }
  x <- q
  st' <- get
  put $ st {nameSupply = st'.nameSupply }
  pure (st' /\ x)

-- | SQL generation internal state.
--   Contains the subqueries and static (i.e. not dependent on any subqueries)
--   restrictions of the query currently being built, as well as a name supply
--   for column renaming.
type GenState =
  { sources         :: Array SQL
  , staticRestricts :: Array (Exp SQL Boolean)
  , groupCols       :: Array (SomeCol SQL)
  , nameSupply      :: Int
  , nameScope       :: Int
  }

-- | Initial state: no subqueries, no restrictions.
initState :: Int -> GenState
initState scope =
  { sources: []
  , staticRestricts: []
  , groupCols: []
  , nameSupply: 0
  , nameScope: scope
  }

renameAll :: forall sql. Array (UntypedCol sql) -> State GenState (Array (SomeCol sql))
renameAll = map join <<< traverse rename

-- | Generate a unique name for the given column.
rename :: forall sql. UntypedCol sql -> State GenState (Array (SomeCol sql))
rename (Untyped col) = do
    n <- freshId
    pure [Named (newName n) col]
  where
    newName ns = runSomeExp col
      case _ of
        Col n -> addColSuffix n $ "_" <> show ns
        _     -> mkColName $ "tmp_" <> show ns


-- | Get a guaranteed unique identifier.
freshId :: State GenState Name
freshId = do
  st <- get
  put $ st { nameSupply = st.nameSupply + 1 }
  pure $ Name st.nameScope st.nameSupply

-- | Get a guaranteed unique column name.
freshName :: State GenState ColName
freshName = do
  n <- freshId
  pure $ mkColName $ "tmp_" <> show n
