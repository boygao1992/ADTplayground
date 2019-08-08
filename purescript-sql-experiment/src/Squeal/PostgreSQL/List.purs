module Squeal.PostgreSQL.List where

import Prelude
import Type.Prelude
import Data.Exists

import Data.Tuple.Nested (type (/\), (/\))
import Type.Data.List (kind List, type (:))
import Type.Data.List as List

class Join (xs :: List) (ys :: List) (zs :: List) | xs ys -> zs
instance jointfNil :: Join List.Nil ys ys
instance jointfCons :: Join xs ys zs => Join (x : xs) ys (x : zs)

class Additional expr where
  also :: forall xs ys zs. Join xs ys zs => expr ys -> expr xs -> expr zs

{- NOTE GADT approximation
1. each constructor carries extra proves e.g. TypeEquals (~)
  - solution: Tagless Final Encoding / smart constructors
2. support existential types
  - solution: Data.Exists
3. exhaustivity checking for pattern matching
  - solution: ADT

data AlignedList p x0 x1 where
  Done :: AlignedList p x x
  (:>>) :: p x0 x1 -> AlignedList p x1 x2 -> AlignedList p x0 x2

-- NOTE Patterns
pattern Done :: forall p x0 x1. (x0 ~ x1) => AlignedList p x0 x1
pattern (:>>) :: forall p x0 x1 x2. p x0 x1 -> AlignedList p x1 x2 -> AlignedList p x0 x2
-}

class AlignedListTFE (alignedList :: (Type -> Type -> Type) -> Type -> Type -> Type) where
  alignedListDone :: forall p x. alignedList p x x
  alignedListCons :: forall p x0 x1 x2. p x0 x1 -> alignedList p x1 x2 -> alignedList p x0 x2
infixr 7 alignedListCons as :>>

data AlignedList (p :: Type -> Type -> Type) x0 x2
  = AlignedListDone
  | AlignedListCons (Exists (AlignedListConsF p x0 x2))
    -- forall x1. AlignedListCons (p x0 x1) (AlignedList p x1 x2)
newtype AlignedListConsF p x0 x2 x1 = AlignedListConsF
  (p x0 x1 /\ AlignedList p x1 x2)
instance alignedListImpl :: AlignedListTFE AlignedList where
  alignedListDone :: forall p x. AlignedList p x x
  alignedListDone = AlignedListDone

  alignedListCons
    :: forall p x0 x1 x2
    . p x0 x1 -> AlignedList p x1 x2 -> AlignedList p x0 x2
  alignedListCons x xs
    = AlignedListCons
    $ mkExists
    $ AlignedListConsF
    $ x /\ xs

