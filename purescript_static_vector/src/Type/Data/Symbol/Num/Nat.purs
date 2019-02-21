module Type.Data.Symbol.Num.Nat where

import Prelude

import Type.Data.Boolean (BProxy(..))
import Type.Data.Boolean as Bool
import Type.Data.Symbol (SProxy(..))
import Type.Data.Symbol as Symbol
import Type.Data.Symbol.Num.Digit (class IsDigit, class IsDigitPred, class Add, toInt) as Digit
import Type.Data.Symbol.Utils (class ReverseSymbol)

-- | Type-level Natural Number

-- | IsNat
class IsNat (n :: Symbol) where
  toInt :: SProxy n -> Int

instance isNatImpl ::
  ( ReverseSymbol n n'
  , IsNatImpl n'
  ) => IsNat n
  where
    toInt _ = toIntImpl (SProxy :: SProxy n')

class IsNatImpl (n :: Symbol) where
  toIntImpl :: SProxy n -> Int

instance isNatImplBase ::
  ( Symbol.Cons h t n
  , IsNatImplDispatch h t
  ) => IsNatImpl n
  where
    toIntImpl _ = toIntImplDispatch (SProxy :: SProxy h) (SProxy :: SProxy t)

class IsNatImplDispatch (h :: Symbol) (t :: Symbol) where
  toIntImplDispatch :: SProxy h -> SProxy t -> Int

instance isNatImplBaseCase ::
  ( Digit.IsDigit h
  ) => IsNatImplDispatch h ""
  where
    toIntImplDispatch _ _ = Digit.toInt (SProxy :: SProxy h)
else instance isNatImplInductionStep ::
  ( Digit.IsDigit h
  , IsNatImpl t
  ) => IsNatImplDispatch h t
  where
    toIntImplDispatch _ _ = Digit.toInt (SProxy :: SProxy h) + toIntImpl (SProxy :: SProxy t) * 10

-- | IsNatPred
class IsNatPred (n :: Symbol) (b :: Bool.Boolean) | n -> b

instance isNatPredEmpty :: IsNatPred "" Bool.False
else instance isNatPredOtherwise ::
  ( Symbol.Cons h t n
  , IsNatPredImpl h t b
  ) => IsNatPred n b

class IsNatPredImpl (h :: Symbol) (t :: Symbol) (b :: Bool.Boolean) | h t -> b

instance isNatImplPredBaseCase ::
  ( Digit.IsDigitPred h b
  ) => IsNatPredImpl h "" b
else instance isNatImplPredInductionStep ::
  ( Digit.IsDigitPred h b1
  , IsNatPred t b2
  , Bool.And b1 b2 b
  ) => IsNatPredImpl h t b

isNatPred :: forall n b. IsNatPred n b => SProxy n -> BProxy b
isNatPred _ = BProxy :: BProxy b

-- | Type-level Arithmetic: Add
class Add (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z

instance addAll ::
  ( IsNat x
  , IsNat y
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
  , AddReversedInductionStep x_h x_t y_h y_t carry z
  ) => AddReversed x y carry z

class AddReversedInductionStep (x_h :: Symbol) (x_t :: Symbol) (y_h :: Symbol) (y_t :: Symbol) (carry :: Symbol) (z :: Symbol) | x_h x_t y_h y_t carry -> z

instance addReversedInductionStepImpl ::
  ( Digit.Add x_h y_h carry1 z1
  , Digit.Add z1 carry0 carry2 z2
  , Digit.Add carry1 carry2 "0" carry3
  , AddReversed x_t y_t carry3 z_rest
  , Symbol.Append z2 z_rest z
  ) => AddReversedInductionStep x_h x_t y_h y_t carry0 z

add :: forall x y z. Add x y z => SProxy x -> SProxy y -> SProxy z
add _ _ = SProxy :: SProxy z

-- | Type-level Arithmetic: Successor

class Succ (pred :: Symbol) (succ :: Symbol) | pred -> succ

instance succImpl ::
  ( IsNat pred
  , Add pred "1" succ
  ) => Succ pred succ

succ :: forall pred succ. Succ pred succ => SProxy pred -> SProxy succ
succ _ = SProxy :: SProxy succ

-- succExample1 :: SProxy "1112"
-- succExample1 = succ $ SProxy :: SProxy "1111"

-- succExample2 :: SProxy "1"
-- succExample2 = succ $ SProxy :: SProxy "0"

