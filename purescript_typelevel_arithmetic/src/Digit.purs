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

-- | Add

class Add (x :: Symbol) (y :: Symbol) (carry :: Symbol) (z :: Symbol) | x y -> carry z

instance add00 :: Add "0" "0" "0" "0"
instance add01 :: Add "0" "1" "0" "1"
instance add02 :: Add "0" "2" "0" "2"
instance add03 :: Add "0" "3" "0" "3"
instance add04 :: Add "0" "4" "0" "4"
instance add05 :: Add "0" "5" "0" "5"
instance add06 :: Add "0" "6" "0" "6"
instance add07 :: Add "0" "7" "0" "7"
instance add08 :: Add "0" "8" "0" "8"
instance add09 :: Add "0" "9" "0" "9"

instance add10 :: Add "1" "0" "0" "1"
instance add11 :: Add "1" "1" "0" "2"
instance add12 :: Add "1" "2" "0" "3"
instance add13 :: Add "1" "3" "0" "4"
instance add14 :: Add "1" "4" "0" "5"
instance add15 :: Add "1" "5" "0" "6"
instance add16 :: Add "1" "6" "0" "7"
instance add17 :: Add "1" "7" "0" "8"
instance add18 :: Add "1" "8" "0" "9"
instance add19 :: Add "1" "9" "1" "0"

instance add20 :: Add "2" "0" "0" "2"
instance add21 :: Add "2" "1" "0" "3"
instance add22 :: Add "2" "2" "0" "4"
instance add23 :: Add "2" "3" "0" "5"
instance add24 :: Add "2" "4" "0" "6"
instance add25 :: Add "2" "5" "0" "7"
instance add26 :: Add "2" "6" "0" "8"
instance add27 :: Add "2" "7" "0" "9"
instance add28 :: Add "2" "8" "1" "0"
instance add29 :: Add "2" "9" "1" "1"

instance add30 :: Add "3" "0" "0" "3"
instance add31 :: Add "3" "1" "0" "4"
instance add32 :: Add "3" "2" "0" "5"
instance add33 :: Add "3" "3" "0" "6"
instance add34 :: Add "3" "4" "0" "7"
instance add35 :: Add "3" "5" "0" "8"
instance add36 :: Add "3" "6" "0" "9"
instance add37 :: Add "3" "7" "1" "0"
instance add38 :: Add "3" "8" "1" "1"
instance add39 :: Add "3" "9" "1" "2"

instance add40 :: Add "4" "0" "0" "4"
instance add41 :: Add "4" "1" "0" "5"
instance add42 :: Add "4" "2" "0" "6"
instance add43 :: Add "4" "3" "0" "7"
instance add44 :: Add "4" "4" "0" "8"
instance add45 :: Add "4" "5" "0" "9"
instance add46 :: Add "4" "6" "1" "0"
instance add47 :: Add "4" "7" "1" "1"
instance add48 :: Add "4" "8" "1" "2"
instance add49 :: Add "4" "9" "1" "3"

instance add50 :: Add "5" "0" "0" "5"
instance add51 :: Add "5" "1" "0" "6"
instance add52 :: Add "5" "2" "0" "7"
instance add53 :: Add "5" "3" "0" "8"
instance add54 :: Add "5" "4" "0" "9"
instance add55 :: Add "5" "5" "1" "0"
instance add56 :: Add "5" "6" "1" "1"
instance add57 :: Add "5" "7" "1" "2"
instance add58 :: Add "5" "8" "1" "3"
instance add59 :: Add "5" "9" "1" "4"

instance add60 :: Add "6" "0" "0" "6"
instance add61 :: Add "6" "1" "0" "7"
instance add62 :: Add "6" "2" "0" "8"
instance add63 :: Add "6" "3" "0" "9"
instance add64 :: Add "6" "4" "1" "0"
instance add65 :: Add "6" "5" "1" "1"
instance add66 :: Add "6" "6" "1" "2"
instance add67 :: Add "6" "7" "1" "3"
instance add68 :: Add "6" "8" "1" "4"
instance add69 :: Add "6" "9" "1" "5"

instance add70 :: Add "7" "0" "0" "7"
instance add71 :: Add "7" "1" "0" "8"
instance add72 :: Add "7" "2" "0" "9"
instance add73 :: Add "7" "3" "1" "0"
instance add74 :: Add "7" "4" "1" "1"
instance add75 :: Add "7" "5" "1" "2"
instance add76 :: Add "7" "6" "1" "3"
instance add77 :: Add "7" "7" "1" "4"
instance add78 :: Add "7" "8" "1" "5"
instance add79 :: Add "7" "9" "1" "6"

instance add80 :: Add "8" "0" "0" "8"
instance add81 :: Add "8" "1" "0" "9"
instance add82 :: Add "8" "2" "1" "0"
instance add83 :: Add "8" "3" "1" "1"
instance add84 :: Add "8" "4" "1" "2"
instance add85 :: Add "8" "5" "1" "3"
instance add86 :: Add "8" "6" "1" "4"
instance add87 :: Add "8" "7" "1" "5"
instance add88 :: Add "8" "8" "1" "6"
instance add89 :: Add "8" "9" "1" "7"

instance add90 :: Add "9" "0" "0" "9"
instance add91 :: Add "9" "1" "1" "0"
instance add92 :: Add "9" "2" "1" "1"
instance add93 :: Add "9" "3" "1" "2"
instance add94 :: Add "9" "4" "1" "3"
instance add95 :: Add "9" "5" "1" "4"
instance add96 :: Add "9" "6" "1" "5"
instance add97 :: Add "9" "7" "1" "6"
instance add98 :: Add "9" "8" "1" "7"
instance add99 :: Add "9" "9" "1" "8"
