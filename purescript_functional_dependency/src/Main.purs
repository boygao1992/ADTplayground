module Main where

import Prelude
import Prim.RowList (kind RowList)
import Prim.RowList as Row
import Type.Data.RowList (RLProxy)
-- import Type.Equality (class TypeEquals)

import Data.List.Lazy.Types (List)
import Data.List.Lazy.Types (nil) as List

-- | TypeEquals
class TypeEquals a b | a -> b, b -> a where
  to :: a -> b
  from :: b -> a

instance refl :: TypeEquals a a where
  to a = a
  from a = a

-- | iterate RowList
class Values (xs :: RowList) (row :: # Type) a
  | xs -> row a
  where
    values :: RLProxy xs -> { | row } -> List a

-- instance nilValues :: Values Row.Nil () a where
--   values _ _ = List.nil

instance nilValues :: ( TypeEquals { | row } {} ) => Values Row.Nil row a where
  values _ _ = List.nil
