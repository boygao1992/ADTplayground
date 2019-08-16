module Selda.Unsafe where

import Prelude
import Type.Prelude

import Selda.Column
import Selda.SqlType
import Selda.Exp
import Selda.Exp as Exp

-- | Cast a column to another type, using whichever coercion semantics are used
--   by the underlying SQL implementation.
cast :: forall s a b. SqlType b => Col s a -> Col s b
cast = liftC $ Exp.cast (sqlType (Proxy :: Proxy b))

-- | A unary operation. Note that the provided function name is spliced
--   directly into the resulting SQL query. Thus, this function should ONLY
--   be used to implement well-defined functions that are missing from Selda's
--   standard library, and NOT in an ad hoc manner during queries.
fun :: forall s a b. String -> Col s a -> Col s b
fun = liftC <<< Exp.unOp <<< Exp.fun

-- | Like 'fun', but with two arguments.
fun2 :: forall s a b c. String -> Col s a -> Col s b -> Col s c
fun2 = liftC2 <<< Exp.fun2
