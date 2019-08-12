module Selda.SqlRow where

import Prelude
import Type.Prelude

import Data.Generic.Rep (class Generic, from, to)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested
import Data.Array as Array
import Control.Monad.State
import Data.Variant.Internal

import Selda.SqlType

newtype ResultReader a = R (State (Array SqlValue) a)
derive newtype instance functorResultReader :: Functor ResultReader
derive newtype instance applyResultReader :: Apply ResultReader
derive newtype instance applicativeResultReader :: Applicative ResultReader
derive newtype instance bindResultReader :: Bind ResultReader
derive newtype instance monadResultReader :: Monad ResultReader

runResultReader :: forall a. ResultReader a -> Array SqlValue -> a
runResultReader (R m) = evalState m

-- NOTE next :: Partial => ResultReader SqlValue
next :: ResultReader (Maybe SqlValue)
next = R <<< state $ \s -> case Array.uncons s of
  Nothing -> Nothing /\ s
  Just { head, tail } -> Just head /\ tail

-- class Typeable a => SqlRow a where
class SqlRow a where
  -- | Read the next, potentially composite, result from a stream of columns.
  nextResult :: ResultReader a
  -- default nextResult :: (Generic a, GSqlRow (Rep a)) => ResultReader a
  -- nextResult = to <$> gNextResult

  -- | The number of nested columns contained in this type.
  nestedCols :: Proxy a -> Int
  -- default nestedCols :: (Generic a, GSqlRow (Rep a)) => Proxy a -> Int
  -- nestedCols _ = gNestedCols (Proxy :: Proxy (Rep a))

instance sqlRowDefault :: (Generic a rep, GSqlRow rep) => SqlRow a where
  nextResult = to <$> gNextResult
  nestedCols _ = gNestedCols (Proxy :: Proxy rep)

-- * Generic derivation for SqlRow
{- class GSqlRow f where
  gNextResult :: ResultReader (f x)
  gNestedCols :: Proxy f -> Int
-}
class GSqlRow rep where
  gNextResult :: ResultReader rep
  gNestedCols :: Proxy rep -> Int

-- TODO instance SqlType a => GSqlRow (K1 i a) where
-- TODO instance GSqlRow f => GSqlRow (M1 c i f) where
-- TODO instance (GSqlRow a, GSqlRow b) => GSqlRow (a :*: b) where
{- TODO
instance
  (TL.TypeError
   ( 'TL.Text "Selda currently does not support creating tables from sum types."
     'TL.:$$:
     'TL.Text "Restrict your table type to a single data constructor."
   )) => GSqlRow (a :+: b) where
-}

-- * Various instances
-- TODO instance SqlRow a => SqlRow (Maybe a) where
-- TODO instances for nested tuples

