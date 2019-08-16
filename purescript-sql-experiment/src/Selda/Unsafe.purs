module Selda.Unsafe where

import Prelude
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

import Selda.Column (Col(..), liftC, liftC2)
import Selda.Exp as Exp
import Selda.Inner (Aggr, Inner, liftAggr)
import Selda.SqlType (class SqlType, sqlType)

-- | Cast a column to another type, using whichever coercion semantics are used
--   by the underlying SQL implementation.
cast :: forall s a b. SqlType b => Col s a -> Col s b
cast = liftC $ Exp.cast (sqlType (Proxy :: Proxy b))

-- | Cast an aggregate to another type, using whichever coercion semantics
--   are used by the underlying SQL implementation.
castAggr :: forall s a b. SqlType b => Aggr s a -> Aggr s b
castAggr = liftAggr cast

-- | Sink the given function into an inner scope.
--
--   Be careful not to use this function with functions capturing rows or columns
--   from an outer scope. For instance, the following usage will likely
--   lead to disaster:
--
-- > query $ do
-- >   x <- #age `from` select person
-- >   inner $ sink (\p -> x + (p ! #age)) <$> select person
--
--   Really, if you have to use this function, ONLY do so in the global scope.
sink :: forall f s a b. (f s a -> f s b) -> f (Inner s) a -> f (Inner s) b
sink = unsafeCoerce

-- | Like 'sink', but with two arguments.
sink2
  :: forall f s a b c
  . (f s a -> f s b -> f s c)
  -> f (Inner s) a -> f (Inner s) b -> f (Inner s) c
sink2 = unsafeCoerce

-- | A unary operation. Note that the provided function name is spliced
--   directly into the resulting SQL query. Thus, this function should ONLY
--   be used to implement well-defined functions that are missing from Selda's
--   standard library, and NOT in an ad hoc manner during queries.
fun :: forall s a b. String -> Col s a -> Col s b
fun = liftC <<< Exp.unOp <<< Exp.fun

-- | Like 'fun', but with two arguments.
fun2 :: forall s a b c. String -> Col s a -> Col s b -> Col s c
fun2 = liftC2 <<< Exp.fun2

-- | A custom operator. @operator "~>" a b@ will compile down to
--   @a ~> b@, with parentheses around @a@ and @b@ iff they are not atomic.
--   This means that SQL operator precedence is disregarded, as all
--   subexpressions are parenthesized. In the following example for instance,
--   @foo a b c@ will compile down to @(a ~> b) ~> c@.
--
-- > (~>) = operator "~>"
-- > infixl 5 ~>
-- > foo a b c = a ~> b ~> c
operator :: forall s a b c. String -> Col s a -> Col s b -> Col s c
operator = liftC2 <<< Exp.binOp <<< Exp.customOp

-- | Like 'fun', but with zero arguments.
fun0 :: forall s a. String -> Col s a
fun0 = One <<< Exp.nulOp <<< Exp.fun0
