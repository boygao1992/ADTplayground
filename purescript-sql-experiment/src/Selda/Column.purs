module Selda.Column where

import Prelude
import Prim.TypeError (class Fail, Text)
import Effect.Exception.Unsafe (unsafeThrow)
import Type.Equality (class TypeEquals)

import Data.Newtype (class Newtype)

import Selda.Exp (Exp(..), UntypedCol, binOp)
import Selda.Exp as Exp
import Selda.SQL (SQL)
import Selda.SqlType (class SqlType, mkLit)
-- import Selda.SqlRow
import Selda.Types (ColName)

-- | Any column tuple.
class Columns a where
  toTup :: Array ColName -> a
  fromTup :: a -> Array (UntypedCol SQL)

-- TODO instance (SqlType a, Columns b) => Columns (Col s a :*: b) where
-- TODO instance (SqlRow a, Columns b) => Columns (Row s a :*: b) where
-- TODO instance Columns (Col s a) where
-- TODO instance Columns (Row s a) where

-- | A database column. A column is often a literal column table, but can also
--   be an expression over such a column or a constant expression.
newtype Col s a = One (Exp SQL a)
derive instance newtypeCol :: Newtype (Col s a) _

-- | A database row. A row is a collection of one or more columns.
newtype Row s a = Many (Array (UntypedCol SQL))
derive instance newtypeRow :: Newtype (Row s a) _

-- | A literal expression.
literal :: forall s a. SqlType a => a -> Col s a
literal = One <<< Lit <<< mkLit

-- TODO instance IsString (Col s Text) where


{- NOTE problem of `class Same s t | s -> t, t -> s`
cannot reuse ifThenElse, solver will force unification (s ~ t)
1. matchNull call ifThenElse with type variables filled (s:=s, t:=s, u:=s a:=a)
2. Same (s:=s) (t:=s) forces evaluation of Some s t
3. `s` and `t` being separately defined type variables at the moment don't carry enough information to coincide with the duplex fundeps from Same s t
DONE may delay unification by adding an extra type class in the middle

Reference
[13.14. Equality constraints, Coercible, and the kind Constraint](https://downloads.haskell.org/~ghc/8.6.5/docs/html/users_guide/glasgow_exts.html#equality-constraints-coercible-and-the-kind-constraint)
-}

-- | Denotes that scopes @s@ and @t@ are identical.
-- NOTE seems this type class is only used to display a comprehensible error message
-- NOTE TypeEquals carries duplex fundeps which enforces type equality
--      Same serves as a lazy step which delays the equality check
-- NOTE mod
class TypeEquals s t <= Same s t where
  to :: forall a. Col s a -> Col t a
  from :: forall a. Col t a -> Col s a

instance sameRefl :: Same s s where
  to s = s
  from s = s
else
instance sameNot
  :: ( Fail (Text "An identifier from an outer scope may not be used in an inner query.")
    , TypeEquals s t
    )
  => Same s t where
  to _ = unsafeThrow "An identifier from an outer scope may not be used in an inner query."
  from _ = unsafeThrow "An identifier from an outer scope may not be used in an inner query."

liftC :: forall s a b. (Exp SQL a -> Exp SQL b) -> Col s a -> Col s b
liftC f (One x) = One (f x)

-- NOTE mod
liftC2
  :: forall s a b c
  . (Exp SQL a -> Exp SQL b -> Exp SQL c)
  -> Col s a -> Col s b -> Col s c
liftC2 f (One a) (One b) = One $ f a b

-- NOTE mod
liftC3
  :: forall s a b c d
  . (Exp SQL a -> Exp SQL b -> Exp SQL c -> Exp SQL d)
  -> Col s a -> Col s b -> Col s c -> Col s d
liftC3 f (One a) (One b) (One c) = One $ f a b c

-- NOTE instance (SqlType a, Num a) => Num (Col s a) where
instance semiringCol :: (Semiring a, SqlType a) => Semiring (Col s a) where
  add = liftC2 $ binOp Exp.add
  zero = literal zero
  mul = liftC2 $ binOp Exp.mul
  one = literal one
instance ringCol :: (Ring a, SqlType a) => Ring (Col s a) where
  sub = liftC2 $ binOp Exp.sub
instance commutativeRingCol :: (CommutativeRing a, SqlType a) => CommutativeRing (Col s a)
-- NOTE instance Fractional (Col s Double) where
-- NOTE instance Fractional (Col s Int) where
instance euclideanRingCol :: (EuclideanRing a, SqlType a) => EuclideanRing (Col s a) where
  degree _ = unsafeThrow "unsupported operator: degree" -- NOTE
  div = liftC2 $ binOp Exp.div
  mod _ _ = unsafeThrow "unsupported operator: mod" -- NOTE

