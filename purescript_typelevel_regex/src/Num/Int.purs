module Num.Int where

import Prelude
import Type.Data.Symbol (SProxy(..))
import Type.Data.Symbol as Symbol
import Type.Data.Boolean (BProxy(..), True, False)
import Type.Data.Boolean as Bool
import Num.Nat as Nat


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
  , IsSign h
  , Nat.IsNat t
  ) => IsInt num
else instance isIntNoSign ::
  Nat.IsNat num => IsInt num

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
instance isNegativeEmpty :: IsNegativePred "" Bool.False
else instance isNegativePredNonEmpty ::
  ( Symbol.Cons h t num -- h t <- num
  , IsMinusPred h b
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

-- | Add
class Add (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z

instance addMatchSign ::
  ( IsNegativePred x x_neg
  , IsNegativePred y y_neg
  , AddDispatch x_neg y_neg x y z
  ) => Add x y z

class AddDispatch (x_neg :: Bool.Boolean) (y_neg :: Bool.Boolean) (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z

-- x, y, z >= 0
class AddImpl (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z

-- | Minus
-- | x :: Nat, y :: Nat, z :: Int
class Minus (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z

-- x, y >= 0
-- z \in Int
class MinusImpl (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z
