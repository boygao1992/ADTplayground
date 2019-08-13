module Selda.Unsafe where

import Prelude
import Type.Prelude

import Selda.Column
import Selda.SqlType
import Selda.Exp

-- | Cast a column to another type, using whichever coercion semantics are used
--   by the underlying SQL implementation.
cast :: forall s a b. SqlType b => Col s a -> Col s b
cast = liftC $ cast_ (sqlType (Proxy :: Proxy b))
