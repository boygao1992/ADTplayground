module Selda where

import Prelude
import Type.Prelude (Proxy(..))

import Data.Maybe (Maybe)
import Data.Generic.Rep (class Generic)
import Effect.Exception.Unsafe (unsafeThrow)
import Unsafe.Coerce (unsafeCoerce)

import Selda.Column
import Selda.Column (from, to) as Same
import Selda.Exp
import Selda.Generic
import Selda.Inner
import Selda.Query
import Selda.Query.Type
import Selda.Selectors
import Selda.SqlType
import Selda.Unsafe


-- | Any column type that can be used with the 'min_' and 'max_' functions.
class SqlType a <= SqlOrd a
-- TODO instances

-- | Wrapper for single column tables.
--   Use this when you need a table with only a single column, with 'table' or
--   'selectValues'.
newtype Only a = Only a
-- TODO instances
-- TODO instance SqlType a => SqlRow (Only a)
-- TODO instance Fail (..) => SqlType (Only a) where

-- | Generate a new random UUID using the system's random number generator.
--   UUIDs generated this way are (astronomically likely to be) unique,
--   but not necessarily unpredictable.
--
--   For applications where unpredictability is crucial, take care to use a
--   proper cryptographic PRNG to generate your UUIDs.
-- TODO newUuid :: MonadEffect m => m UUID

-- | Annotation to force the type of a polymorphic label (i.e. @#foo@) to
--   be a selector. This is useful, for instance, when defining unique
--   constraints: @sel #foo :- unique@.
-- TODO sel :: forall t a. Selector t a -> Selector t a

-- | Add the given column to the column pointed to by the given selector.
plusEquals
  :: forall s t a
  . SqlType a
  => Semiring (Col s a)
  => Selector t a -> Col s a -> Assignment s t
plusEquals s c = s $= (_ + c)
infixl 2 plusEquals as +=

-- | Subtract the given column from the column pointed to by the given selector.
minusEquals
  :: forall s t a
  . SqlType a
  => Ring (Col s a)
  => Selector t a -> Col s a -> Assignment s t
minusEquals s c = s $= (_ - c)
infixl 2 minusEquals as -=

-- | Multiply the column pointed to by the given selector, by the given column.
multiplyEquals
  :: forall s t a
  . SqlType a
  => Semiring (Col s a)
  => Selector t a -> Col s a -> Assignment s t
multiplyEquals s c = s $= (_ * c)
infixl 2 multiplyEquals as *=

-- | Logically @OR@ the column pointed to by the given selector with
--   the given column.
orEquals
  :: forall s t a
  . SqlType a
  => HeytingAlgebra (Col s a)
  => Selector t a -> Col s a -> Assignment s t
orEquals s c = s $= (_ || c)
infixl 2 orEquals as ||=

-- | Logically @AND@ the column pointed to by the given selector with
--   the given column.
andEquals
  :: forall s t a
  . SqlType a
  => HeytingAlgebra (Col s a)
  => Selector t a -> Col s a -> Assignment s t
andEquals s c = s $= (_ && c)
infixl 2 andEquals as &&=

class The a theOnly | a -> theOnly where
  -- | Extract the value of a row from a singleton table.
  the :: a -> theOnly

instance theOnly :: The (Only a) a where
  the (Only x) = x

instance theRow :: The (Row s (Only a)) (Col s a) where
  the (Many [Untyped exp]) = runSomeExp exp \x -> One (unsafeCoerce x) -- NOTE
  the (Many _)           = unsafeThrow "BUG: non-singleton Only-column"

-- | Create a singleton table column from an appropriate value.
only :: forall s a. SqlType a => Col s a -> Row s (Only a)
only (One x)  = Many [untyped x]


-- | Create a new column with the given fields.
--   Any unassigned fields will contain their default values.
new :: forall s a rep. Generic a rep => GRelation rep => Array (Assignment s a) -> Row s a
new fields = Many (gNew (Proxy :: Proxy rep)) `with` fields

-- | Convenient shorthand for @fmap (! sel) q@.
--   The following two queries are quivalent:
--
-- > q1 = name `from` select people
-- > q2 = do
-- >   person <- select people
-- >   return (person ! name)
from
  :: forall s t a
  . SqlType a
  => Selector t a
  -> Query s (Row s t)
  -> Query s (Col s a)
from s q = (_ ! s) <$> q -- NOTE unsafe
-- infixr 7 `from`

-- | Explicitly create an inner query. Equivalent to @innerJoin (const true)@.
--
--   Sometimes it's handy, for performance
--   reasons and otherwise, to perform a subquery and restrict only that query
--   before adding the result of the query to the result set, instead of first
--   adding the query to the result set and restricting the whole result set
--   afterwards.
inner
  :: forall s a oc
  . OuterCols a oc
  => Columns a
  => Columns oc
  => Query (Inner s) a
  -> Query s oc
inner = innerJoin (const true_)

-- | Create and filter an inner query, before adding it to the current result
--   set.
--
--   @q `suchThat` p@ is generally more efficient than
--   @select q >>= \x -> restrict (p x) >> pure x@.
suchThat
  :: forall s a oc
  . OuterCols a oc
  => Columns a
  => Columns oc
  => Query (Inner s) a
  -> (a -> Col (Inner s) Boolean)
  -> Query s oc
suchThat q p = inner $ do
  x <- q
  restrict (p x)
  pure x
-- infixr 7 `suchThat`

-- | Comparisons over columns.
--   Note that when comparing nullable (i.e. @Maybe@) columns, SQL @NULL@
--   semantics are used. This means that comparing to a @NULL@ field will remove
--   the row in question from the current set.
--   To test for @NULL@, use 'isNull' instead of @.== literal Nothing@.
-- NOTE mod
equals :: forall s t a. Same s t => SqlType a => Col s a -> Col t a -> Col s Boolean
equals cs1 ct2 = (liftC2 $ binOp Eq) cs1 (Same.from ct2)
notEquals :: forall s t a. Same s t => SqlType a => Col s a -> Col t a -> Col s Boolean
notEquals cs1 ct2 = (liftC2 $ binOp Neq) cs1 (Same.from ct2)
greaterThan :: forall s t a. Same s t => SqlOrd a => Col s a -> Col t a -> Col s Boolean
greaterThan cs1 ct2 = (liftC2 $ binOp Gt) cs1 (Same.from ct2)
lessThan :: forall s t a. Same s t => SqlOrd a => Col s a -> Col t a -> Col s Boolean
lessThan cs1 ct2 = (liftC2 $ binOp Lt) cs1 (Same.from ct2)
greaterEqual :: forall s t a. Same s t => SqlOrd a => Col s a -> Col t a -> Col s Boolean
greaterEqual cs1 ct2 = (liftC2 $ binOp Gte) cs1 (Same.from ct2)
lessEqual :: forall s t a. Same s t => SqlOrd a => Col s a -> Col t a -> Col s Boolean
lessEqual cs1 ct2 = (liftC2 $ binOp Lte) cs1 (Same.from ct2)
infixl 4 equals as .==
infixl 4 notEquals as ./=
infixl 4 greaterThan as .>
infixl 4 lessThan as .<
infixl 4 greaterEqual as .>=
infixl 4 lessEqual as .<=

-- | Is the given column null?
isNull :: forall s a. SqlType a => Col s (Maybe a) -> Col s Boolean
isNull = liftC $ unOp IsNull

-- | Applies the given function to the given nullable column where it isn't null,
--   and returns the given default value where it is.
--
--   This is the Selda equivalent of 'maybe'.
-- NOTE mod
matchNull
  :: forall s t a b
  . SqlType a
  => SqlType b
  => Same s t
  => Col s b
  -> (Col s a -> Col s b)
  -> Col t (Maybe a)
  -> Col s b
matchNull nullvalue f x =
  ifThenElse (isNull $ Same.from x)
    nullvalue
    (f $ cast $ Same.from x)

-- | True and false boolean literals.
true_ :: forall s. Col s Boolean
true_ = literal true
false_ :: forall s. Col s Boolean
false_ = literal false

-- | Perform a conditional on a column
-- NOTE mod
ifThenElse
  :: forall s t u a
  . Same s t
  => Same s u
  => SqlType a
  => Col s Boolean -> Col t a -> Col u a -> Col s a
ifThenElse cs1 ct2 cu3 = (liftC3 if_) cs1 (Same.from ct2) (Same.from cu3)
