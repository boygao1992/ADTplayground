module Selda.Inner where

import Prelude
import Prim.TypeError (class Fail, Above, Text)

import Data.Array ((:))
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (dimap)

import Selda.Column (Col(..), Row)
import Selda.Exp (Exp, UntypedCol, aggrEx, untyped)
import Selda.SQL (SQL)
import Selda.SqlType (class SqlType)
import Selda.Types (type (:*:), (:*:))


-- | A single aggregate column.
--   Aggregate columns may not be used to restrict queries.
--   When returned from an 'aggregate' subquery, an aggregate column is
--   converted into a non-aggregate column.
newtype Aggr s a = Aggr (Exp SQL a)
derive instance newtypeAggr :: Newtype (Aggr s a) _

-- | Lift a function over columns to aggregates.
liftAggr :: forall s a b. (Col s a -> Col s b) -> Aggr s a -> Aggr s b
liftAggr = dimap (wrap <<< unwrap) (wrap <<< unwrap)


-- | Denotes an inner query.
--   For aggregation, treating sequencing as the cartesian product of queries
--   does not work well.
--   Instead, we treat the sequencing of 'aggregate' with other
--   queries as the cartesian product of the aggregated result of the query,
--   a small but important difference.
--
--   However, for this to work, the aggregate query must not depend on any
--   columns in the outer product. Therefore, we let the aggregate query be
--   parameterized over @Inner s@ if the parent query is parameterized over @s@,
--   to enforce this separation.
data Inner s

-- | Create a named aggregate function.
--   Like 'fun', this function is generally unsafe and should ONLY be used
--   to implement missing backend-specific functionality.
aggr :: forall s a b. SqlType a => String -> Col s a -> Aggr s b
aggr f (One x)  = Aggr (aggrEx f x)

-- | Convert one or more inner column to equivalent columns in the outer query.
--   @OuterCols (Aggr (Inner s) a :*: Aggr (Inner s) b) = Col s a :*: Col s b@,
--   for instance.
-- type family OuterCols a where
--   OuterCols (Col (Inner s) a :*: b)  = Col s a :*: OuterCols b
--   OuterCols (Col (Inner s) a)        = Col s a
--   OuterCols (Row (Inner s) a :*: b)  = Row s a :*: OuterCols b
--   OuterCols (Row (Inner s) a)        = Row s a
--   OuterCols (Col s a) = TypeError
--     ( 'TL.Text "An inner query can only return rows and columns from its own scope."
--     )
--   OuterCols (Row s a) = TypeError
--     ( 'TL.Text "An inner query can only return rows and columns from its own scope."
--     )
--   OuterCols a = TypeError
--     ( 'TL.Text "Only (inductive tuples of) row and columns can be returned from" ':$$:
--       'TL.Text "an inner query."
--     )
class OuterCols i o | i -> o
instance outerColsColInduction
  :: OuterCols b o => OuterCols (Col (Inner s) (a :*: b)) (Col s (a :*: o))
else
instance outerColsColBase
  :: OuterCols (Col (Inner s) a) (Col s a)
else
instance outerColsRowInduction
  :: OuterCols b o => OuterCols (Row (Inner s) (a :*: b)) (Row s (a :*: o))
else
instance outerColsRowBase
  :: OuterCols (Row (Inner s) a) (Row s a)
else
instance outerColsRowError
  :: Fail
  ( Text "An inner query can only return rows and columns from its own scope.")
  => OuterCols (Row s a) o
else
instance outerColsOtherwise
  :: Fail
  ( Above
    (Text "Only (inductive tuples of) row and columns can be returned from")
    (Text "an inner query.")
  ) => OuterCols i o

-- type family AggrCols a where
--   AggrCols (Aggr (Inner s) a :*: b) = Col s a :*: AggrCols b
--   AggrCols (Aggr (Inner s) a)       = Col s a
--   AggrCols (Aggr s a) = TypeError
--     ( 'TL.Text "An aggregate query can only return columns from its own" ':$$:
--       'TL.Text "scope."
--     )
--   AggrCols a = TypeError
--     ( 'TL.Text "Only (inductive tuples of) aggregates can be returned from" ':$$:
--       'TL.Text "an aggregate query."
--     )
class AggrCols i o | i -> o
instance aggrColsAggrInduction
  :: AggrCols b o => AggrCols (Aggr (Inner s) (a :*: b)) (Col s (a :*: o))
else
instance aggrColsAggrBase
  :: AggrCols (Aggr (Inner s) a) (Col s a)
else
instance aggrColsAggrError
  :: Fail (Text "An aggregate query can only return columns from its own scope.")
  => AggrCols (Aggr s a) o
else
instance aggrColsOtherwise
  :: Fail
  ( Above
    (Text "Only (inductive tuples of) aggregates can be returned from")
    (Text "an aggregate query.")
  )
  => AggrCols i o


-- | The results of a left join are always nullable, as there is no guarantee
--   that all joined columns will be non-null.
--   @JoinCols a@ where @a@ is an extensible tuple is that same tuple, but in
--   the outer query and with all elements nullable.
--   For instance:
--
-- >  LeftCols (Col (Inner s) Int :*: Col (Inner s) Text)
-- >    = Col s (Maybe Int) :*: Col s (Maybe Text)
-- type family LeftCols a where
--   LeftCols (Col (Inner s) (Maybe a) :*: b) = Col s (Maybe a) :*: LeftCols b
--   LeftCols (Col (Inner s) a :*: b)         = Col s (Maybe a) :*: LeftCols b
--   LeftCols (Col (Inner s) (Maybe a))       = Col s (Maybe a)
--   LeftCols (Col (Inner s) a)               = Col s (Maybe a)

--   LeftCols (Row (Inner s) (Maybe a) :*: b) = Row s (Maybe a) :*: LeftCols b
--   LeftCols (Row (Inner s) a :*: b)         = Row s (Maybe a) :*: LeftCols b
--   LeftCols (Row (Inner s) (Maybe a))       = Row s (Maybe a)
--   LeftCols (Row (Inner s) a)               = Row s (Maybe a)
--   LeftCols a = TypeError
--     ( 'TL.Text "Only (inductive tuples of) rows and columns can be returned" ':$$:
--       'TL.Text "from a join."
--     )

class LeftCols i o | i -> o
instance leftColsColInductionMaybe
  :: LeftCols b o
  => LeftCols (Col (Inner s) ((Maybe a) :*: b)) (Col s ((Maybe a) :*: o))
else
instance leftColsColInduction
  :: LeftCols b o
  => LeftCols (Col (Inner s) (a :*: b)) (Col s (a :*: o))
else
instance leftColsColBaseMaybe
  :: LeftCols (Col (Inner s) (Maybe a)) (Col s (Maybe a))
else
instance leftColsColBase
  :: LeftCols (Col (Inner s) a) (Col s a)
else
instance leftColsRowInductionMaybe
  :: LeftCols b o
  => LeftCols (Row (Inner s) ((Maybe a) :*: b)) (Row s ((Maybe a) :*: o))
else
instance leftColsRowInduction
  :: LeftCols b o
  => LeftCols (Row (Inner s) (a :*: b)) (Row s (a :*: o))
else
instance leftColsRowBaseMaybe
  :: LeftCols (Row (Inner s) (Maybe a)) (Row s (Maybe a))
else
instance leftColsRowBase
  :: LeftCols (Row (Inner s) a) (Row s a)
else
instance leftColsOtherwise
  :: Fail
  ( Above
    (Text "Only (inductive tuples of) rows and columns can be returned")
    (Text "from a join.")
  )
  => LeftCols i o


-- | One or more aggregate columns.
class Aggregates a where
  unAggrs :: a -> Array (UntypedCol SQL)
instance aggregatesAggrInduction
  :: Aggregates b => Aggregates ((Aggr (Inner s) a) :*: b) where
  unAggrs ((Aggr a) :*: b) = untyped a : unAggrs b
instance aggregatesAggrBase :: Aggregates (Aggr (Inner s) a) where
  unAggrs (Aggr x) = [untyped x]
