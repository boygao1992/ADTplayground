module Selda.Column where

import Prelude

import Data.Newtype (class Newtype)

import Selda.Exp (Exp(..), UntypedCol)
import Selda.SQL (SQL)
import Selda.SqlType (class SqlType, mkLit)
-- import Selda.SqlRow
import Selda.Types (ColName)
import Type.Equality (class TypeEquals)


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

liftC3
  :: forall s a b c d
  .(Exp SQL a -> Exp SQL b -> Exp SQL c -> Exp SQL d)
  -> Col s a
  -> Col s b
  -> Col s c
  -> Col s d
liftC3 f (One a) (One b) (One c) = One (f a b c)

-- | Denotes that scopes @s@ and @t@ are identical.
class TypeEquals s t <= Same s t | s -> t, t -> s where
  liftC2
    :: forall a b c
    . (Exp SQL a -> Exp SQL b -> Exp SQL c)
    -> Col s a -> Col t b -> Col s c
  -- liftC2 f (One a) (One b) = One (f a b)

defaultLiftC2
  :: forall s t a b c
  . (Exp SQL a -> Exp SQL b -> Exp SQL c)
  -> Col s a -> Col t b -> Col s c
defaultLiftC2 f (One a) (One b) = One (f a b)

instance sameRefl :: Same s s where
  liftC2 = defaultLiftC2

{- TODO
instance (s ~ t, TypeError ('TL.Text "An identifier from an outer scope may not be used in an inner query.")) => Same s t
-}

liftC :: forall s a b. (Exp SQL a -> Exp SQL b) -> Col s a -> Col s b
liftC f (One x) = One (f x)

-- TODO instance (SqlType a, Num a) => Num (Col s a) where
-- TODO instance Fractional (Col s Double) where
-- TODO instance Fractional (Col s Int) where
