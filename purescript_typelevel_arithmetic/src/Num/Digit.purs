module Num.Digit where

import Type.Data.Symbol (SProxy)
import Type.Data.Boolean (BProxy(..))
import Type.Data.Boolean as Boolean
import Type.Data.Ordering as Ord

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

-- | Compare
class Compare (x :: Symbol) (y :: Symbol) (o :: Ord.Ordering) | x y -> o

instance compare00 :: Compare "0" "0" Ord.EQ
instance compare01 :: Compare "0" "1" Ord.LT
instance compare02 :: Compare "0" "2" Ord.LT
instance compare03 :: Compare "0" "3" Ord.LT
instance compare04 :: Compare "0" "4" Ord.LT
instance compare05 :: Compare "0" "5" Ord.LT
instance compare06 :: Compare "0" "6" Ord.LT
instance compare07 :: Compare "0" "7" Ord.LT
instance compare08 :: Compare "0" "8" Ord.LT
instance compare09 :: Compare "0" "9" Ord.LT
instance compare10 :: Compare "1" "0" Ord.GT
instance compare11 :: Compare "1" "1" Ord.EQ
instance compare12 :: Compare "1" "2" Ord.LT
instance compare13 :: Compare "1" "3" Ord.LT
instance compare14 :: Compare "1" "4" Ord.LT
instance compare15 :: Compare "1" "5" Ord.LT
instance compare16 :: Compare "1" "6" Ord.LT
instance compare17 :: Compare "1" "7" Ord.LT
instance compare18 :: Compare "1" "8" Ord.LT
instance compare19 :: Compare "1" "9" Ord.LT
instance compare20 :: Compare "2" "0" Ord.GT
instance compare21 :: Compare "2" "1" Ord.GT
instance compare22 :: Compare "2" "2" Ord.EQ
instance compare23 :: Compare "2" "3" Ord.LT
instance compare24 :: Compare "2" "4" Ord.LT
instance compare25 :: Compare "2" "5" Ord.LT
instance compare26 :: Compare "2" "6" Ord.LT
instance compare27 :: Compare "2" "7" Ord.LT
instance compare28 :: Compare "2" "8" Ord.LT
instance compare29 :: Compare "2" "9" Ord.LT
instance compare30 :: Compare "3" "0" Ord.GT
instance compare31 :: Compare "3" "1" Ord.GT
instance compare32 :: Compare "3" "2" Ord.GT
instance compare33 :: Compare "3" "3" Ord.EQ
instance compare34 :: Compare "3" "4" Ord.LT
instance compare35 :: Compare "3" "5" Ord.LT
instance compare36 :: Compare "3" "6" Ord.LT
instance compare37 :: Compare "3" "7" Ord.LT
instance compare38 :: Compare "3" "8" Ord.LT
instance compare39 :: Compare "3" "9" Ord.LT
instance compare40 :: Compare "4" "0" Ord.GT
instance compare41 :: Compare "4" "1" Ord.GT
instance compare42 :: Compare "4" "2" Ord.GT
instance compare43 :: Compare "4" "3" Ord.GT
instance compare44 :: Compare "4" "4" Ord.EQ
instance compare45 :: Compare "4" "5" Ord.LT
instance compare46 :: Compare "4" "6" Ord.LT
instance compare47 :: Compare "4" "7" Ord.LT
instance compare48 :: Compare "4" "8" Ord.LT
instance compare49 :: Compare "4" "9" Ord.LT
instance compare50 :: Compare "5" "0" Ord.GT
instance compare51 :: Compare "5" "1" Ord.GT
instance compare52 :: Compare "5" "2" Ord.GT
instance compare53 :: Compare "5" "3" Ord.GT
instance compare54 :: Compare "5" "4" Ord.GT
instance compare55 :: Compare "5" "5" Ord.EQ
instance compare56 :: Compare "5" "6" Ord.LT
instance compare57 :: Compare "5" "7" Ord.LT
instance compare58 :: Compare "5" "8" Ord.LT
instance compare59 :: Compare "5" "9" Ord.LT
instance compare60 :: Compare "6" "0" Ord.GT
instance compare61 :: Compare "6" "1" Ord.GT
instance compare62 :: Compare "6" "2" Ord.GT
instance compare63 :: Compare "6" "3" Ord.GT
instance compare64 :: Compare "6" "4" Ord.GT
instance compare65 :: Compare "6" "5" Ord.GT
instance compare66 :: Compare "6" "6" Ord.EQ
instance compare67 :: Compare "6" "7" Ord.LT
instance compare68 :: Compare "6" "8" Ord.LT
instance compare69 :: Compare "6" "9" Ord.LT
instance compare70 :: Compare "7" "0" Ord.GT
instance compare71 :: Compare "7" "1" Ord.GT
instance compare72 :: Compare "7" "2" Ord.GT
instance compare73 :: Compare "7" "3" Ord.GT
instance compare74 :: Compare "7" "4" Ord.GT
instance compare75 :: Compare "7" "5" Ord.GT
instance compare76 :: Compare "7" "6" Ord.GT
instance compare77 :: Compare "7" "7" Ord.EQ
instance compare78 :: Compare "7" "8" Ord.LT
instance compare79 :: Compare "7" "9" Ord.LT
instance compare80 :: Compare "8" "0" Ord.GT
instance compare81 :: Compare "8" "1" Ord.GT
instance compare82 :: Compare "8" "2" Ord.GT
instance compare83 :: Compare "8" "3" Ord.GT
instance compare84 :: Compare "8" "4" Ord.GT
instance compare85 :: Compare "8" "5" Ord.GT
instance compare86 :: Compare "8" "6" Ord.GT
instance compare87 :: Compare "8" "7" Ord.GT
instance compare88 :: Compare "8" "8" Ord.EQ
instance compare89 :: Compare "8" "9" Ord.LT
instance compare90 :: Compare "9" "0" Ord.GT
instance compare91 :: Compare "9" "1" Ord.GT
instance compare92 :: Compare "9" "2" Ord.GT
instance compare93 :: Compare "9" "3" Ord.GT
instance compare94 :: Compare "9" "4" Ord.GT
instance compare95 :: Compare "9" "5" Ord.GT
instance compare96 :: Compare "9" "6" Ord.GT
instance compare97 :: Compare "9" "7" Ord.GT
instance compare98 :: Compare "9" "8" Ord.GT
instance compare99 :: Compare "9" "9" Ord.EQ



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

-- Minus
-- x, y, z \in { "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"}
-- carry \in { "0", "1" }, meaning { "-0", "-1" }
class Minus (x :: Symbol) (y :: Symbol) (carry :: Symbol) (z :: Symbol) | x y -> carry z

instance minus00 :: Minus "0" "0" "0" "0"
instance minus01 :: Minus "0" "1" "1" "9"
instance minus02 :: Minus "0" "2" "1" "8"
instance minus03 :: Minus "0" "3" "1" "7"
instance minus04 :: Minus "0" "4" "1" "6"
instance minus05 :: Minus "0" "5" "1" "5"
instance minus06 :: Minus "0" "6" "1" "4"
instance minus07 :: Minus "0" "7" "1" "3"
instance minus08 :: Minus "0" "8" "1" "2"
instance minus09 :: Minus "0" "9" "1" "1"
instance minus10 :: Minus "1" "0" "0" "1"
instance minus11 :: Minus "1" "1" "0" "0"
instance minus12 :: Minus "1" "2" "1" "9"
instance minus13 :: Minus "1" "3" "1" "8"
instance minus14 :: Minus "1" "4" "1" "7"
instance minus15 :: Minus "1" "5" "1" "6"
instance minus16 :: Minus "1" "6" "1" "5"
instance minus17 :: Minus "1" "7" "1" "4"
instance minus18 :: Minus "1" "8" "1" "3"
instance minus19 :: Minus "1" "9" "1" "2"
instance minus20 :: Minus "2" "0" "0" "2"
instance minus21 :: Minus "2" "1" "0" "1"
instance minus22 :: Minus "2" "2" "0" "0"
instance minus23 :: Minus "2" "3" "1" "9"
instance minus24 :: Minus "2" "4" "1" "8"
instance minus25 :: Minus "2" "5" "1" "7"
instance minus26 :: Minus "2" "6" "1" "6"
instance minus27 :: Minus "2" "7" "1" "5"
instance minus28 :: Minus "2" "8" "1" "4"
instance minus29 :: Minus "2" "9" "1" "3"
instance minus30 :: Minus "3" "0" "0" "3"
instance minus31 :: Minus "3" "1" "0" "2"
instance minus32 :: Minus "3" "2" "0" "1"
instance minus33 :: Minus "3" "3" "0" "0"
instance minus34 :: Minus "3" "4" "1" "9"
instance minus35 :: Minus "3" "5" "1" "8"
instance minus36 :: Minus "3" "6" "1" "7"
instance minus37 :: Minus "3" "7" "1" "6"
instance minus38 :: Minus "3" "8" "1" "5"
instance minus39 :: Minus "3" "9" "1" "4"
instance minus40 :: Minus "4" "0" "0" "4"
instance minus41 :: Minus "4" "1" "0" "3"
instance minus42 :: Minus "4" "2" "0" "2"
instance minus43 :: Minus "4" "3" "0" "1"
instance minus44 :: Minus "4" "4" "0" "0"
instance minus45 :: Minus "4" "5" "1" "9"
instance minus46 :: Minus "4" "6" "1" "8"
instance minus47 :: Minus "4" "7" "1" "7"
instance minus48 :: Minus "4" "8" "1" "6"
instance minus49 :: Minus "4" "9" "1" "5"
instance minus50 :: Minus "5" "0" "0" "5"
instance minus51 :: Minus "5" "1" "0" "4"
instance minus52 :: Minus "5" "2" "0" "3"
instance minus53 :: Minus "5" "3" "0" "2"
instance minus54 :: Minus "5" "4" "0" "1"
instance minus55 :: Minus "5" "5" "0" "0"
instance minus56 :: Minus "5" "6" "1" "9"
instance minus57 :: Minus "5" "7" "1" "8"
instance minus58 :: Minus "5" "8" "1" "7"
instance minus59 :: Minus "5" "9" "1" "6"
instance minus60 :: Minus "6" "0" "0" "6"
instance minus61 :: Minus "6" "1" "0" "5"
instance minus62 :: Minus "6" "2" "0" "4"
instance minus63 :: Minus "6" "3" "0" "3"
instance minus64 :: Minus "6" "4" "0" "2"
instance minus65 :: Minus "6" "5" "0" "1"
instance minus66 :: Minus "6" "6" "0" "0"
instance minus67 :: Minus "6" "7" "1" "9"
instance minus68 :: Minus "6" "8" "1" "8"
instance minus69 :: Minus "6" "9" "1" "7"
instance minus70 :: Minus "7" "0" "0" "7"
instance minus71 :: Minus "7" "1" "0" "6"
instance minus72 :: Minus "7" "2" "0" "5"
instance minus73 :: Minus "7" "3" "0" "4"
instance minus74 :: Minus "7" "4" "0" "3"
instance minus75 :: Minus "7" "5" "0" "2"
instance minus76 :: Minus "7" "6" "0" "1"
instance minus77 :: Minus "7" "7" "0" "0"
instance minus78 :: Minus "7" "8" "1" "9"
instance minus79 :: Minus "7" "9" "1" "8"
instance minus80 :: Minus "8" "0" "0" "8"
instance minus81 :: Minus "8" "1" "0" "7"
instance minus82 :: Minus "8" "2" "0" "6"
instance minus83 :: Minus "8" "3" "0" "5"
instance minus84 :: Minus "8" "4" "0" "4"
instance minus85 :: Minus "8" "5" "0" "3"
instance minus86 :: Minus "8" "6" "0" "2"
instance minus87 :: Minus "8" "7" "0" "1"
instance minus88 :: Minus "8" "8" "1" "0"
instance minus89 :: Minus "8" "9" "1" "9"
instance minus90 :: Minus "9" "0" "0" "9"
instance minus91 :: Minus "9" "1" "0" "8"
instance minus92 :: Minus "9" "2" "0" "7"
instance minus93 :: Minus "9" "3" "0" "6"
instance minus94 :: Minus "9" "4" "0" "5"
instance minus95 :: Minus "9" "5" "0" "4"
instance minus96 :: Minus "9" "6" "0" "3"
instance minus97 :: Minus "9" "7" "0" "2"
instance minus98 :: Minus "9" "8" "1" "1"
instance minus99 :: Minus "9" "9" "1" "0"
