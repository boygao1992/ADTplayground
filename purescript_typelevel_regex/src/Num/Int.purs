module Num.Int where

import Prelude

import Num.Digit as Digit
import Num.Nat as Nat
import Symbol.Utils (class ReverseSymbol)
import Type.Data.Boolean (BProxy(..), True, False)
import Type.Data.Boolean as Bool
import Type.Data.Ordering (OProxy(..), LT, GT, EQ)
import Type.Data.Ordering as Ord
import Type.Data.Symbol (SProxy(..))
import Type.Data.Symbol as Symbol


-- | IsSign
class IsSign (sign :: Symbol)

instance isSignPositive :: IsSign "+"
instance isSignNegative :: IsSign "-"

-- | IsSignPred
class IsSignPred (sign :: Symbol) (b :: Bool.Boolean) | sign -> b

instance isSignPredPositive :: IsSignPred "+" Bool.True
else instance isSignPredNegative :: IsSignPred "-" Bool.True
else instance isSignNotSign :: IsSignPred notSign Bool.False

-- | IsInt
class IsInt (num :: Symbol)

instance isIntHasSign ::
  ( Symbol.Cons h t num -- h t <- i
  , IsSignPred h isSign
  , IsIntDispatch isSign t num
  ) => IsInt num

class IsIntDispatch (isSign :: Bool.Boolean) (t :: Symbol) (num :: Symbol)

instance isIntDispatchHasSign ::
  ( Nat.IsNat t
  ) => IsIntDispatch Bool.True t num
else instance isIntDispatchNoSign ::
  ( Nat.IsNat num
  ) => IsIntDispatch Bool.False t num

-- | IsIntPred
class IsIntPred (num :: Symbol) (b :: Bool.Boolean) | num -> b

instance isIntPredEmpty :: IsIntPred "" Bool.False
else instance isIntPredNonEmpty ::
  ( Symbol.Cons h t num
  , IsSignPred h hasSign
  , Nat.IsNatPred t t_isNat
  , Bool.And hasSign t_isNat case1
  , Nat.IsNatPred num num_isNat
  , Bool.Or case1 num_isNat b -- b = (hasSign && t_isNat) || num_isNat
  ) => IsIntPred num b

isIntPred :: forall i o. IsIntPred i o => SProxy i -> BProxy o
isIntPred _ = BProxy :: BProxy o

-- Test
-- isIntPredExample1 :: BProxy True
-- isIntPredExample1 = isIntPred $ SProxy :: SProxy "12"
-- isIntPredExample2 :: BProxy True
-- isIntPredExample2 = isIntPred $ SProxy :: SProxy "-12"
-- isIntPredExample3 :: BProxy True
-- isIntPredExample3 = isIntPred $ SProxy :: SProxy "+12"
-- isIntPredExample4 :: BProxy False
-- isIntPredExample4 = isIntPred $ SProxy :: SProxy "++12"
-- isIntPredExample5 :: BProxy False
-- isIntPredExample5 = isIntPred $ SProxy :: SProxy "1-2"

-- | IsNegativePred
class IsNegativePred (num :: Symbol) (b :: Bool.Boolean) | num -> b

-- TODO rule out minus zero
-- solution: class Normalize
-- 1. remove extra "0"s from beginning
-- 2. remove "-" from "-0"
instance isNegativeEmpty :: IsNegativePred "" Bool.False
else instance isNegativePredNonEmpty ::
  ( Symbol.Cons h t num -- h t <- num
  , IsMinusPred h isMinus
  , Nat.IsNatPred t isNat
  , Bool.And isMinus isNat b
  ) => IsNegativePred num b

class IsMinusPred (sign :: Symbol) (b :: Bool.Boolean) | sign -> b

instance isMinusPredMinus :: IsMinusPred "-" Bool.True
else instance isMinusOtherwise :: IsMinusPred sign Bool.False

isNegativePred :: forall i o. IsNegativePred i o => SProxy i -> BProxy o
isNegativePred _ = BProxy :: BProxy o

-- Test
-- isNegativePredExample1 :: BProxy False
-- isNegativePredExample1 = isNegativePred (SProxy :: SProxy "")
-- isNegativePredExample2 :: BProxy True
-- isNegativePredExample2 = isNegativePred (SProxy :: SProxy "-12")

-- TODO exception: minus zero (-0, -00, -000, ...) is not negative
-- isNegativePredExample3 :: BProxy True
-- isNegativePredExample3 = isNegativePred (SProxy :: SProxy "-00")

-- | Inverse
class Inverse (x :: Symbol) (inv :: Symbol) | x -> inv

instance negateMatchSign ::
  ( IsInt x
  , IsNegativePred x isNeg
  , InverseDispatch isNeg x inv
  ) => Inverse x inv

class InverseDispatch (b :: Bool.Boolean) (x :: Symbol) (inv :: Symbol) | b x -> inv

instance negateDispatchNegative ::
  ( Symbol.Cons sign num x -- sign num <- x
  ) => InverseDispatch Bool.True x num
else instance negateDispatchPositive ::
  ( Symbol.Cons "-" x num -- "-" x -> num
  ) => InverseDispatch Bool.False x num

inverse :: forall i o. Inverse i o => SProxy i -> SProxy o
inverse _ = SProxy :: SProxy o

-- Test
-- inverseExample1 :: SProxy "1"
-- inverseExample1 = inverse (SProxy :: SProxy "-1")
-- inverseExample2 :: SProxy "-99"
-- inverseExample2 = inverse (SProxy :: SProxy "99")


-- | Compare
class Compare (x :: Symbol) (y :: Symbol) (o :: Ord.Ordering) | x y -> o

instance compareImpl ::
  ( IsInt x
  , IsInt y
  , IsNegativePred x x_isNeg
  , IsNegativePred y y_isNeg
  , CompareDispatch x_isNeg y_isNeg x y o
  ) => Compare x y o

class CompareDispatch (x_isNeg :: Bool.Boolean) (y_isNeg :: Bool.Boolean) (x :: Symbol) (y :: Symbol) (o :: Ord.Ordering) | x y -> o

instance comparePosNeg :: CompareDispatch Bool.False Bool.True x y Ord.GT
else instance compareNegPos :: CompareDispatch Bool.True Bool.False x y Ord.LT
else instance comparePosPos ::
  ( CompareNonNegative x y o
  ) => CompareDispatch Bool.False Bool.False x y o
else instance compareNegNeg ::
  ( Inverse x_neg x_pos
  , Inverse y_neg y_pos
  , CompareNonNegative x_pos y_pos o'
  , Ord.Invert o' o
  ) => CompareDispatch Bool.True Bool.True x_neg y_neg o

class CompareNonNegative (x :: Symbol) (y :: Symbol) (o :: Ord.Ordering) | x y -> o

instance compareNonNegativeImpl ::
  ( ReverseSymbol x x'
  , ReverseSymbol y y'
  , CompareNonNegativeReversed x' y' o
  ) => CompareNonNegative x y o

class CompareNonNegativeReversed (x :: Symbol) (y :: Symbol) (o :: Ord.Ordering) | x y -> o

instance compareNonNegativeReversedBaseCase1 ::
  CompareNonNegativeReversed "" "" Ord.EQ
else instance compareNonNegativeReversedBaseCase2 ::
  CompareNonNegativeReversed "" y Ord.LT
else instance compareNonNegativeReversedBaseCase3 ::
  CompareNonNegativeReversed x "" Ord.GT
else instance compareNonNegativeReversedInductionStep ::
  ( Symbol.Cons x_h x_t x
  , Symbol.Cons y_h y_t y
  , CompareNonNegativeReversed x_t y_t o1
  , Digit.Compare x_h y_h o2
  , Ord.Append o1 o2 o
  ) => CompareNonNegativeReversed x y o

compare :: forall a b o. Compare a b o => SProxy a -> SProxy b -> OProxy o
compare _ _ = OProxy :: OProxy o

-- Test
-- compareExample1 :: OProxy LT
-- compareExample1 = compare (SProxy :: SProxy "1") (SProxy :: SProxy "2")
-- compareExample2 :: OProxy GT
-- compareExample2 = compare (SProxy :: SProxy "100") (SProxy :: SProxy "99")
-- compareExample3 :: OProxy LT
-- compareExample3 = compare (SProxy :: SProxy "-100") (SProxy :: SProxy "99")
-- compareExample4 :: OProxy GT
-- compareExample4 = compare (SProxy :: SProxy "100") (SProxy :: SProxy "-99")
-- compareExample5 :: OProxy LT
-- compareExample5 = compare (SProxy :: SProxy "-100") (SProxy :: SProxy "-99")
-- compareExample6 :: OProxy EQ
-- compareExample6 = compare (SProxy :: SProxy "-100") (SProxy :: SProxy "-100")
-- compareExample7 :: OProxy EQ
-- compareExample7 = compare (SProxy :: SProxy "100") (SProxy :: SProxy "100")

-- | Add
class Add (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z

instance addMatchSign ::
  ( IsInt x
  , IsInt y
  , IsNegativePred x x_isNeg
  , IsNegativePred y y_isNeg
  , AddDispatch x_isNeg y_isNeg x y z
  ) => Add x y z

class AddDispatch (x_isNeg :: Bool.Boolean) (y_isNeg :: Bool.Boolean) (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z

instance addPosPos ::
  ( Nat.Add x_pos y_pos z -- x y -> z
  ) => AddDispatch Bool.False Bool.False x_pos y_pos z
else instance addNegNeg :: -- (-x) + (-y) = -(x + y)
  ( Inverse x_neg x_pos -- x_neg -> x_pos
  , Inverse y_neg y_pos -- y_neg -> y_pos
  , Nat.Add x_pos y_pos z_pos -- x_pos y_pos -> z_pos
  , Inverse z_pos z -- z_pos -> z
  ) => AddDispatch Bool.True Bool.True x_neg y_neg z
else instance addPosNeg :: -- x + (-y) = x - y
  ( Inverse y_neg y_pos -- y_neg -> y_pos
  , MinusImpl x_pos y_pos z -- x_pos y_pos -> z
  ) => AddDispatch Bool.False Bool.True x_pos y_neg z
else instance addNegPos :: -- (-x) + y = -(x - y)
  ( Inverse x_neg x_pos -- x_neg -> x_pos
  , MinusImpl y_pos x_pos z' -- y_pos x_pos -> z_neg
  , Inverse z' z -- z_neg -> z
  ) => AddDispatch Bool.True Bool.False x_neg y_pos z

-- | Minus
class Minus (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z

instance minusMatchSign ::
  ( IsInt x
  , IsInt y
  , IsNegativePred x x_isNeg
  , IsNegativePred y y_isNeg
  , MinusDispatch x_isNeg y_isNeg x y z
  ) => Minus x y z

class MinusDispatch (x_isNeg :: Bool.Boolean) (y_isNeg :: Bool.Boolean) (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z

instance minusPosPos ::
  ( MinusImpl x_pos y_pos z
  ) => MinusDispatch Bool.False Bool.False x_pos y_pos z
else instance minusNegNeg :: -- (-x) - (-y) = -(x - y)
  ( Inverse x_neg x_pos
  , Inverse y_neg y_pos
  , MinusImpl x_pos y_pos z'
  , Inverse z' z
  ) => MinusDispatch Bool.True Bool.True x_neg y_neg z
else instance minusPosNeg :: -- x - (-y) = x + y
  ( Inverse y_neg y_pos
  , Nat.Add x_pos y_pos z
  ) => MinusDispatch Bool.False Bool.True x_pos y_neg z
else instance minusNegPos :: -- (-x) - y = - (x + y)
  ( Inverse x_neg x_pos
  , Nat.Add x_pos y_pos z_pos
  , Inverse z_pos z
  ) => MinusDispatch Bool.True Bool.False x_neg y_pos z

-- x, y >= 0
-- z \in Int
class MinusImpl (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z

instance minusImpl :: -- TODO compare x and y; or potentially back track with a negative carrying number
  ( ReverseSymbol x x'
  , ReverseSymbol y y'
  , MinusReversed x' y' "0" z'
  , ReverseSymbol z' z
  ) => MinusImpl x y z

class MinusReversed (x :: Symbol) (y :: Symbol) (carry :: Symbol) (z :: Symbol) | x y carry -> z
