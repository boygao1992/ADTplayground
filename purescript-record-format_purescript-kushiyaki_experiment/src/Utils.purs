module Utils where

import Prelude

import Type.Data.Symbol (SProxy(..))
import Type.Data.Symbol as Symbol
import Type.Data.Boolean (BProxy(..))
import Type.Data.Boolean as Boolean

-- | Remove Space
class RemoveSpace (i :: Symbol) (o :: Symbol) | i -> o

instance removeSpaceEmpty :: RemoveSpace "" ""
else instance removeSpaceAll ::
  ( Symbol.Cons h t i -- h t <- i
  , RemoveSpaceImpl h t o -- h t -> o
  ) => RemoveSpace i o

class RemoveSpaceImpl (h :: Symbol) (t :: Symbol) (o :: Symbol) | h t ->o

instance removeSpaceImplInductionStepSpace ::
  ( RemoveSpace t o -- t -> o
  ) => RemoveSpaceImpl " " t o
else instance removeSpaceImplInductionStepNonSpace ::
  ( RemoveSpace t rest -- t -> rest
  , Symbol.Append h rest o -- h rest -> o
  ) => RemoveSpaceImpl h t o

removeSpace :: forall i o. RemoveSpace i o => SProxy i -> SProxy o
removeSpace _ = SProxy :: SProxy o

-- | ReverseSymbol

class ReverseSymbol (i :: Symbol) (o :: Symbol) | i -> o

instance reverseSymbolEmpty :: ReverseSymbol "" ""
else instance reverseSymbolOtherwise ::
  (Symbol.Cons h t i -- h t <- i
  , ReverseSymbolImpl h t o -- h t -> o
  ) => ReverseSymbol i o

class ReverseSymbolImpl (h :: Symbol) (t :: Symbol) (o :: Symbol) | h t -> o

instance reverseSymbolImplInductionStep ::
  ( ReverseSymbol t rest -- t -> rest
  , Symbol.Append rest h o -- rest h -> o
  ) => ReverseSymbolImpl h t o

reverseSymbol :: forall i o. ReverseSymbol i o => SProxy i -> SProxy o
reverseSymbol _ = SProxy :: SProxy o

-- | Type-level Digit

class Digit (d :: Symbol)

instance numberZero :: Digit "0"
instance numberOne :: Digit "1"
instance numberTwo :: Digit "2"
instance numberThree :: Digit "3"
instance numberFour :: Digit "4"
instance numberFive :: Digit "5"
instance numberSix :: Digit "6"
instance numberSeven :: Digit "7"
instance numberEight :: Digit "8"
instance numberNine :: Digit "9"

class IsDigit (d :: Symbol) (b :: Boolean.Boolean) | d -> b

instance isDigitZero :: IsDigit "0" Boolean.True
else instance isDigitOne :: IsDigit "1" Boolean.True
else instance isDigitTwo :: IsDigit "2" Boolean.True
else instance isDigitThree :: IsDigit "3" Boolean.True
else instance isDigitFour :: IsDigit "4" Boolean.True
else instance isDigitFive :: IsDigit "5" Boolean.True
else instance isDigitSix :: IsDigit "6" Boolean.True
else instance isDigitSeven :: IsDigit "7" Boolean.True
else instance isDigitEight :: IsDigit "8" Boolean.True
else instance isDigitNine :: IsDigit "9" Boolean.True
else instance notDigit :: IsDigit d Boolean.False

isDigit :: forall d b. IsDigit d b => SProxy d -> BProxy b
isDigit _ = BProxy :: BProxy b

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
