module Type.Data.List where

import Type.Data.Boolean as Bool
import Type.Proxy (Proxy)
import Type.Utils as Type

foreign import kind List
foreign import data Nil :: List
foreign import data Cons :: Type -> List -> List

infix 3 type Cons as :

data LProxy (list :: List) = LProxy

-- | ContainsPred
class ContainsPred (list :: List) a (b :: Bool.Boolean) | list a -> b

instance containsPredNil ::
  ContainsPred Nil a Bool.False

instance containsPredCons ::
  ( ContainsPred restList a b0
  , Type.IsEqualPred typ a b1
  , Bool.Or b0 b1 b
  ) => ContainsPred (Cons typ restList) a b

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

-- Test
set :: forall typ i o. Set typ i o => Proxy typ -> LProxy i -> LProxy o
set _ _ = LProxy :: LProxy o
