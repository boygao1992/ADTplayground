module Number where

import Prelude hiding (add)

import Digit (class IsDigit, class IsDigitPred, class Add) as Digit
import Type.Data.Boolean (BProxy(..))
import Type.Data.Boolean as Boolean
import Type.Data.Symbol (SProxy(..))
import Type.Data.Symbol as Symbol
import Utils (class ReverseSymbol)

-- | Type-level Number

-- | IsNumber
class IsNumber (n :: Symbol)

instance isNumberMultiDigits ::
  ( Symbol.Cons h t n
  , IsNumberImpl h t
  ) => IsNumber n

class IsNumberImpl (h :: Symbol) (t :: Symbol)

instance isNumberImplBaseCase ::
  ( Digit.IsDigit h
  ) => IsNumberImpl h ""
else instance isNumberImplInductionStep ::
  ( Digit.IsDigit h
  , IsNumber t
  ) => IsNumberImpl h t

-- | IsNumberPred
class IsNumberPred (n :: Symbol) (b :: Boolean.Boolean) | n -> b

instance isNumberPredEmpty :: IsNumberPred "" Boolean.False
else instance isNumberPredOtherwise ::
  ( Symbol.Cons h t n
  , IsNumberPredImpl h t b
  ) => IsNumberPred n b

class IsNumberPredImpl (h :: Symbol) (t :: Symbol) (b :: Boolean.Boolean) | h t -> b

instance isNumberImplPredBaseCase ::
  ( Digit.IsDigitPred h b
  ) => IsNumberPredImpl h "" b
else instance isNumberImplPredInductionStep ::
  ( Digit.IsDigitPred h b1
  , IsNumberPred t b2
  , Boolean.And b1 b2 b
  ) => IsNumberPredImpl h t b

isNumberPred :: forall n b. IsNumberPred n b => SProxy n -> BProxy b
isNumberPred _ = BProxy :: BProxy b

-- | Type-level Arithmetic: Add
class Add (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z

instance addAll ::
  ( IsNumber x
  , IsNumber y
  , ReverseSymbol x x'
  , ReverseSymbol y y'
  , AddReversed x' y' "0" z'
  , ReverseSymbol z' z
  ) => Add x y z

class AddReversed (x :: Symbol) (y :: Symbol) (carry :: Symbol) (z :: Symbol) | x y carry -> z

instance addReversedBaseCase1 :: AddReversed "" y "0" y
else instance addReversedBaseCase2 :: AddReversed x "" "0" x
else instance addReversedBaseCase3 ::
  ( AddReversed "1" y "0" z
  ) => AddReversed "" y "1" z
else instance addReversedBaseCase4 ::
  ( AddReversed x "1" "0" z
  ) => AddReversed x "" "1" z
else instance addReversedInductionStep ::
  ( Symbol.Cons x_h x_t x
  , Symbol.Cons y_h y_t y
  , AddImpl x_h x_t y_h y_t carry z
  ) => AddReversed x y carry z

class AddImpl (x_h :: Symbol) (x_t :: Symbol) (y_h :: Symbol) (y_t :: Symbol) (carry :: Symbol) (z :: Symbol) | x_h x_t y_h y_t carry -> z

instance addImplInductionStep ::
  ( Digit.Add x_h y_h carry1 z1
  , Digit.Add z1 carry0 carry2 z2
  , Digit.Add carry1 carry2 "0" carry3
  , AddReversed x_t y_t carry3 z_rest
  , Symbol.Append z2 z_rest z
  ) => AddImpl x_h x_t y_h y_t carry0 z

add :: forall x y z. Add x y z => SProxy x -> SProxy y -> SProxy z
add _ _ = SProxy :: SProxy z
