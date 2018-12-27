module Number where

import Prelude

import Type.Data.Symbol (SProxy)
import Type.Data.Symbol as Symbol
import Type.Data.Boolean (BProxy(..))
import Type.Data.Boolean as Boolean

import Digit (class IsDigit, class IsDigitPred)
import Utils (class ReverseSymbol)

-- | Type-level Number

-- | IsNumber
class IsNumber (n :: Symbol)

instance isNumberSingleDigit ::
  (IsDigit n
  ) => IsNumber n
else instance isNumberMultiDigits ::
  ( Symbol.Cons h t n
  , IsDigit h
  , IsNumber t
  ) => IsNumber n

-- | IsNumberPred
class IsNumberPred (n :: Symbol) (b :: Boolean.Boolean) | n -> b

instance isNumberEmpty :: IsNumberPred "" Boolean.False
else instance isNumberOtherwise ::
  ( Symbol.Cons h t n
  , IsNumberPredImpl h t b
  ) => IsNumberPred n b

class IsNumberPredImpl (h :: Symbol) (t :: Symbol) (b :: Boolean.Boolean) | h t -> b

instance isNumberImplBaseCase ::
  ( IsDigitPred h b
  ) => IsNumberPredImpl h "" b
else instance isNumberImplInductionStep ::
  ( IsDigitPred h b1
  , IsNumberPred t b2
  , Boolean.And b1 b2 b
  ) => IsNumberPredImpl h t b

isNumberPred :: forall n b. IsNumberPred n b => SProxy n -> BProxy b
isNumberPred _ = BProxy :: BProxy b

-- | Type-level Arithmetic: Add
class Add (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z

instance addAll ::
  ( ReverseSymbol x x'
  , ReverseSymbol y y'
  , AddReversed x' y' z'
  , ReverseSymbol z' z
  ) => Add x y z

class AddReversed (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z

class AddImpl (x_h :: Symbol) (x_t :: Symbol) (y_h :: Symbol) (y_t :: Symbol) (z :: Symbol) | x_h x_t y_h y_t -> z
