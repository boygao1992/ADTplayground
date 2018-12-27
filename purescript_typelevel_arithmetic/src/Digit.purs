module Digit where

import Prelude

import Type.Data.Symbol (SProxy)
import Type.Data.Boolean (BProxy(..))
import Type.Data.Boolean as Boolean

-- | Type-level Digit

class IsDigit (d :: Symbol)

instance numberZero :: IsDigit "0"
instance numberOne :: IsDigit "1"
instance numberTwo :: IsDigit "2"
instance numberThree :: IsDigit "3"
instance numberFour :: IsDigit "4"
instance numberFive :: IsDigit "5"
instance numberSix :: IsDigit "6"
instance numberSeven :: IsDigit "7"
instance numberEight :: IsDigit "8"
instance numberNine :: IsDigit "9"

class IsDigitPred (d :: Symbol) (b :: Boolean.Boolean) | d -> b

instance isDigitZero :: IsDigitPred "0" Boolean.True
else instance isDigitOne :: IsDigitPred "1" Boolean.True
else instance isDigitTwo :: IsDigitPred "2" Boolean.True
else instance isDigitThree :: IsDigitPred "3" Boolean.True
else instance isDigitFour :: IsDigitPred "4" Boolean.True
else instance isDigitFive :: IsDigitPred "5" Boolean.True
else instance isDigitSix :: IsDigitPred "6" Boolean.True
else instance isDigitSeven :: IsDigitPred "7" Boolean.True
else instance isDigitEight :: IsDigitPred "8" Boolean.True
else instance isDigitNine :: IsDigitPred "9" Boolean.True
else instance notDigit :: IsDigitPred d Boolean.False

isDigitPred :: forall d b. IsDigitPred d b => SProxy d -> BProxy b
isDigitPred _ = BProxy :: BProxy b
