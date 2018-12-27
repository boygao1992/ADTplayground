module Digit where

import Prelude

import Type.Data.Symbol (SProxy)
import Type.Data.Boolean (BProxy(..))
import Type.Data.Boolean as Boolean

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
