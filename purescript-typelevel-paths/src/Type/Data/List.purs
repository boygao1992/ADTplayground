module Type.Data.List where

import Type.Prelude
import Prim.TypeError

import Type.Data.Boolean as B
import Type.Proxy (Proxy)
import Type.Utils as Type

import Data.Generic.Rep
import Data.Generic.Rep.Type.Utils

foreign import kind List
foreign import data Nil :: List
foreign import data Cons :: Type -> List -> List

infixr 3 type Cons as :

data LProxy (list :: List) = LProxy

-- | Singleton
class Singleton (a :: Type) (as :: List) | a -> as
instance singletonImpl :: Singleton a (a : Nil)

-- | Append
class Append (xs :: List) (ys :: List) (zs :: List) | xs ys -> zs
instance appendBase :: Append Nil ys ys
instance appendInduction :: Append xs ys zs => Append (x : xs) ys (x : zs)

-- | ContainsPred
class ContainsPred (list :: List) a (b :: B.Boolean) | list a -> b

instance containsPredNil :: ContainsPred Nil a B.False
instance containsPredCons ::
  ( ContainsPred ts a b0
  , Type.IsEqualPred t a b1
  , B.Or b0 b1 b
  )
  => ContainsPred (t : ts) a b

-- | ContainsNamePred
class ContainsNamePred (list :: List) (name :: Symbol) (b :: B.Boolean) | list name -> b

instance containsNamePredNil :: ContainsNamePred Nil name B.False
instance ocntainsNamePredCons ::
  ( ContainsNamePred ts name b0
  , Generic t tRep
  , GetConstructorName tRep name'
  , Type.IsEqualPred (SProxy name) (SProxy name') b1
  , B.Or b0 b1 b
  )
  => ContainsNamePred (t : ts) name b

-- | GetByName
class GetByName (list :: List) (name :: Symbol) o | list name -> o
instance getByNameNil ::
  Fail (Beside (Text "list doesn't contain type: ") (Text name))
  => GetByName Nil name t
instance getByNameCons ::
  ( Generic t tRep
  , GetConstructorName tRep name'
  , Type.IsEqualPred (SProxy name) (SProxy name') b
  , GetByNameIf b t ts name o
  )
  => GetByName (t : ts) name o

class GetByNameIf (b :: B.Boolean) t (ts :: List) (name :: Symbol) o | b t ts name -> o
instance getByNameIfTrue :: GetByNameIf B.True t ts name t
instance getByNameIfFalse :: GetByName ts name o => GetByNameIf B.False t ts name o

-- | Remove
class Remove typ (i :: List) (o :: List) | typ i -> o

instance removeNil ::
  Remove typ Nil Nil

instance removeConsIsEqual ::
  ( Remove typ restList restO
  ) => Remove typ (Cons typ restList) restO
else instance removeConsNotEqual ::
  ( Remove typ restList restO
  ) => Remove typ (Cons a restList) (Cons a restO)

-- | Set
class Set typ (i :: List) (o :: List) | typ i -> o

instance setImpl ::
  ( Remove typ i o'
  ) => Set typ i (Cons typ o')

-- | Homogeneous
class Homogeneous (xs :: List) x | xs -> x

instance homogeneousNil :: Homogeneous Nil x
instance homogeneousCons :: Homogeneous rest x => Homogeneous (Cons x rest) x

-- Test
set :: forall typ i o. Set typ i o => Proxy typ -> LProxy i -> LProxy o
set _ _ = LProxy :: LProxy o
