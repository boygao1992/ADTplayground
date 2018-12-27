module Number where

import Prelude

import Type.Data.Symbol (SProxy)
import Type.Data.Symbol as Symbol
import Type.Data.Boolean (BProxy(..))
import Type.Data.Boolean as Boolean

import Digit (class IsDigit)

-- | Type-level Number

class IsNumber (n :: Symbol) (b :: Boolean.Boolean) | n -> b

instance isNumberEmpty :: IsNumber "" Boolean.False
else instance isNumberOtherwise ::
  ( Symbol.Cons h t n
  , IsNumberImpl h t b
  ) => IsNumber n b

class IsNumberImpl (h :: Symbol) (t :: Symbol) (b :: Boolean.Boolean) | h t -> b

instance isNumberImplBaseCase ::
  ( IsDigit h b
  ) => IsNumberImpl h "" b
else instance isNumberImplInductionStep ::
  ( IsDigit h b1
  , IsNumber t b2
  , Boolean.And b1 b2 b
  ) => IsNumberImpl h t b

isNumber :: forall n b. IsNumber n b => SProxy n -> BProxy b
isNumber _ = BProxy :: BProxy b

-- | Type-level Arithmetic: Add
class Add (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z
