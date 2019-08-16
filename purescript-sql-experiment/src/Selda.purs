module Selda where

import Prelude
import Type.Prelude (Proxy(..))

import Data.Newtype (unwrap)
import Data.Maybe (Maybe(..))
import Data.Generic.Rep (class Generic)
import Effect.Exception.Unsafe (unsafeThrow)
import Unsafe.Coerce (unsafeCoerce)

import Selda.Column
import Selda.Column (from, to) as Same
import Selda.Exp
import Selda.Exp as Exp
import Selda.Generic
import Selda.Inner
import Selda.Query
import Selda.Query.Type
import Selda.Selectors
import Selda.SqlType
import Selda.SQL
import Selda.Unsafe as Unsafe


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
sel :: forall t a. Selector t a -> Selector t a
sel = identity

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
  the (Many _)             = unsafeThrow "BUG: non-singleton Only-column"

-- | Create a singleton table column from an appropriate value.
only :: forall s a. SqlType a => Col s a -> Row s (Only a)
only (One x) = Many [untyped x]


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
equals :: forall s t a. Same s t => SqlType a => Col s a -> Col t a -> Col s Boolean
equals cs1 ct2 = (liftC2 $ binOp Exp.eq) cs1 (Same.from ct2)
infixl 4 equals as .==

notEquals :: forall s t a. Same s t => SqlType a => Col s a -> Col t a -> Col s Boolean
notEquals cs1 ct2 = (liftC2 $ binOp Exp.neq) cs1 (Same.from ct2)
infixl 4 notEquals as ./=

greaterThan :: forall s t a. Same s t => SqlOrd a => Col s a -> Col t a -> Col s Boolean
greaterThan cs1 ct2 = (liftC2 $ binOp Exp.gt) cs1 (Same.from ct2)
infixl 4 greaterThan as .>

lessThan :: forall s t a. Same s t => SqlOrd a => Col s a -> Col t a -> Col s Boolean
lessThan cs1 ct2 = (liftC2 $ binOp Exp.lt) cs1 (Same.from ct2)
infixl 4 lessThan as .<

greaterEqual :: forall s t a. Same s t => SqlOrd a => Col s a -> Col t a -> Col s Boolean
greaterEqual cs1 ct2 = (liftC2 $ binOp Exp.gte) cs1 (Same.from ct2)
infixl 4 greaterEqual as .>=

lessEqual :: forall s t a. Same s t => SqlOrd a => Col s a -> Col t a -> Col s Boolean
lessEqual cs1 ct2 = (liftC2 $ binOp Exp.lte) cs1 (Same.from ct2)
infixl 4 lessEqual as .<=

-- | Is the given column null?
isNull :: forall s a. SqlType a => Col s (Maybe a) -> Col s Boolean
isNull = liftC $ unOp IsNull

-- | Applies the given function to the given nullable column where it isn't null,
--   and returns the given default value where it is.
--
--   This is the Selda equivalent of 'maybe'.
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
    (f $ Unsafe.cast $ Same.from x)

-- | If the second value is Nothing, return the first value. Otherwise return
--   the second value.
ifNull
  :: forall s t a
  . Same s t
  => SqlType a
  => Col s a -> Col t (Maybe a) -> Col s a
ifNull nullvalue x =
  ifThenElse (isNull $ Same.from x)
    nullvalue
    (Unsafe.cast x)

{- NOTE
class Mappable f where
  type Container f a
  (.<$>) :: (SqlType a, SqlType b)
         => (Col s a -> Col s b)
         -> f s (Container f a)
         -> f s (Container f b)

instance Mappable Aggr where
  type Container Aggr a = a
  (.<$>) = liftAggr

instance Mappable Col where
  type Container Col a = Maybe a
  f .<$> mx = Unsafe.cast (f (Unsafe.cast mx))

-- NOTE solution 1: separate emulated type family
-- problem: MappableContainer instances are not accessible when defining instances for Mappable
-- (hacky) workaround: unsafeCoerce
class MappableContainer (f :: Type -> Type -> Type) i o | f i -> o
instance mappableContainerAggr :: MappableContainer Aggr a a
instance mappableContainerCol :: MappableContainer Col a (Maybe a)

class Mappable (f :: Type -> Type -> Type) where
  fmap
    :: forall s a b ca cb
    . SqlType a
    => SqlType b
    => MappableContainer a ca
    => MappableContainer a cb
    => (Col s a -> Col s b)
    -> f s ca -> f s cb

-- NOTE (ad-hoc) solution 2: use a Functor to encode simple type family
-- problem: `Identity a` is not exactly the same as `a`
-- (expensive) workaround: add Functor instances to Col, Exp, etc.
class Mappable (f :: Type -> Type -> Type) (c :: Type -> Type) | f -> c where
  fmap
    :: forall s a b
    . SqlType a
    => SqlType b
    => MappableContainer a ca
    => MappableContainer a cb
    => (Col s a -> Col s b)
    -> f s (c a) -> f s (c b)
instance mappableAggr :: Mappable Aggr Identity
instance mappableCol :: Mappable Col Maybe

-- NOTE solution 3: incorporate usage of type family directly into the type class with fundeps
-- problem: extra (n + 1) type variables and a fundep for each occurrence of a n-parameter type family
-}

-- | Any container type which can be mapped over.
--   Sort of like 'Functor', if you squint a bit.
class Mappable (f :: Type -> Type -> Type) a b ca cb | f a -> ca, f b -> cb where
  fmap
    :: forall s
    . SqlType a
    => SqlType b
    => (Col s a -> Col s b)
    -> f s ca -> f s cb
infixl 4 fmap as .<$>

instance mappableAggr :: Mappable Aggr a b a b where
  fmap = liftAggr
instance mappableCol :: Mappable Col a b (Maybe a) (Maybe b) where
  fmap f mx = Unsafe.cast (f (Unsafe.cast mx))

-- | Any container type for which we can check object membership.
class Set (set :: Type -> Type) where
  -- | Is the given column contained in the given set?
  isIn :: forall s t a. Same s t => SqlType a => Col s a -> set (Col t a) -> Col s Boolean

instance setArray :: Set Array where
  isIn _ [] = false_
  isIn (One x) xs = One $ inList x $ unwrap <$> xs
-- TODO compQueryWithFreshScope
-- instance Set (Query s) where

and :: forall s t. Same s t => Col s Boolean -> Col t Boolean -> Col s Boolean
and cs1 ct2 = (liftC2 $ binOp Exp.and) cs1 (Same.from ct2)
infixr 3 and as .&&

or :: forall s t. Same s t => Col s Boolean -> Col t Boolean -> Col s Boolean
or cs1 ct2 = (liftC2 $ binOp Exp.or) cs1 (Same.from ct2)
infixr 2 or as .||

-- | Ordering for 'order'.
ascending = Asc :: Order
descending = Desc :: Order

-- | Lift a non-nullable column to a nullable one.
--   Useful for creating expressions over optional columns:
--
-- > data Person = Person {name :: Text, age :: Int, pet :: Maybe Text}
-- >   deriving Generic
-- > instance SqlRow Person
-- >
-- > people :: Table Person
-- > people = table "people" []
-- >
-- > peopleWithCats = do
-- >   person <- select people
-- >   restrict (person ! #pet .== just "cat")
-- >   return (person ! #name)
just :: forall s a. SqlType a => Col s a -> Col s (Maybe a)
just = Unsafe.cast

-- | Returns 'true' if the given field in the given row is equal to the given
--   literal.
is :: forall r s c. SqlType c => Selector r c -> c -> Row s r -> Col s Boolean
is s x r = r ! s .== (literal x :: Col s c)

-- | SQL NULL, at any type you like.
null_ :: forall s a. SqlType a => Col s (Maybe a)
null_ = literal Nothing

-- | Specialization of 'literal' for integers.
int :: forall s. Int -> Col s Int
int = literal

-- | Specialization of 'literal' for doubles.
float :: forall s. Number -> Col s Number
float = literal

-- | Specialization of 'literal' for text.
text :: forall s. String -> Col s String
text = literal

-- | True and false boolean literals.
true_ = literal true :: forall s. Col s Boolean
false_ = literal false :: forall s. Col s Boolean

-- | The SQL @LIKE@ operator; matches strings with @%@ wildcards.
--   For instance:
--
-- > "%gon" `like` "dragon" .== true
like :: forall s t. Same s t => Col s String -> Col t String -> Col s Boolean
like cs1 ct2 = (liftC2 $ binOp Exp.like) cs1 (Same.from ct2)

-- | The number of non-null values in the given column.
count :: forall s a. SqlType a => Col s a -> Aggr s Int
count = aggr "COUNT"

-- | The average of all values in the given column.
avg :: forall s a. SqlType a => EuclideanRing a => Col s a -> Aggr s (Maybe a)
avg = aggr "AVG"

-- | The greatest value in the given column. Texts are compared lexically.
max_ :: forall s a. SqlOrd a => Col s a -> Aggr s (Maybe a)
max_ = aggr "MAX"

-- | The smallest value in the given column. Texts are compared lexically.
min_ :: forall s a. SqlOrd a => Col s a -> Aggr s (Maybe a)
min_ = aggr "MIN"

-- | Sum all values in the given column.
sum_ :: forall a b s. SqlType a => SqlType b => Semiring a => Semiring b => Col s a -> Aggr s b
sum_ = liftAggr (ifNull (zero :: Col s b) <<< Unsafe.cast) <<< aggr "SUM"

-- | Round a value to the nearest integer. Equivalent to @roundTo 0@.
-- TODO cast only when a ~ Int
round_ :: forall s a. SqlType a => EuclideanRing a => Col s Number -> Col s a
round_ = Unsafe.cast <<< Unsafe.fun "ROUND"

-- | Round a column to the given number of decimals places.
roundTo :: forall s. Col s Int -> Col s Number -> Col s Number
roundTo = flip $ Unsafe.fun2 "ROUND"

-- | Calculate the length of a string column.
length_ :: forall s. Col s String -> Col s Int
length_ = Unsafe.fun "LENGTH"

-- | Boolean negation.
not_ :: forall s. Col s Boolean -> Col s Boolean
not_ = liftC $ Exp.unOp Exp.not

-- | Convert a boolean column to any numeric type.
fromBool :: forall s a. SqlType a => EuclideanRing a => Col s Boolean -> Col s a
fromBool = Unsafe.cast

-- | Convert an integer column to any numeric type.
fromInt :: forall s a. SqlType a => EuclideanRing a => Col s Int -> Col s a
fromInt = Unsafe.cast

-- | Convert any SQL type to a string.
toString :: forall s a. SqlType a => Col s a -> Col s String
toString = Unsafe.cast

-- | Perform a conditional on a column
-- NOTE mod
ifThenElse
  :: forall s t u a
  . Same s t
  => Same s u
  => SqlType a
  => Col s Boolean -> Col t a -> Col u a -> Col s a
ifThenElse cs1 ct2 cu3 = (liftC3 if_) cs1 (Same.from ct2) (Same.from cu3)
