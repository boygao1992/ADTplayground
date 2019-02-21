module Type.Data.Symbol.Num.Int where

import Prelude

import Type.Data.Boolean (BProxy(..))
import Type.Data.Boolean as Bool
import Type.Data.Ordering as Ord
import Type.Data.Symbol (SProxy(..))
import Type.Data.Symbol as Symbol
import Type.Data.Symbol.Num.Digit as Digit
import Type.Data.Symbol.Num.Nat as Nat
import Type.Data.Symbol.Utils (class ReverseSymbol)

-- | IsSign
class IsSign (sign :: Symbol)

instance isSignPositive :: IsSign "+"
instance isSignNegative :: IsSign "-"

-- | IsSignPred
class IsSignPred (sign :: Symbol) (b :: Bool.Boolean) | sign -> b

instance isSignPredPositive :: IsSignPred "+" Bool.True
else instance isSignPredNegative :: IsSignPred "-" Bool.True
else instance isSignNotSign :: IsSignPred notSign Bool.False

-- | HasSign
class HasSignPred (num :: Symbol) (b :: Bool.Boolean) | num -> b

instance hasSignPredImpl ::
  ( Symbol.Cons h t num -- h t <- i
  , IsSignPred h b
  ) => HasSignPred num b

-- | IsInt
class IsInt (num :: Symbol) where
  toInt :: SProxy num -> Int

instance isIntHasSign ::
  ( HasSignPred num hasSign
  , IsIntDispatch hasSign num
  ) => IsInt num
  where
    toInt _ = toIntDispatch (BProxy :: BProxy hasSign) (SProxy :: SProxy num)

class IsIntDispatch (hasSign :: Bool.Boolean) (num :: Symbol) where
  toIntDispatch :: BProxy hasSign -> SProxy num -> Int

instance isIntDispatchHasSign ::
  ( Symbol.Cons h t num -- h t <- num
  , IsMinusPred h isMinus
  , IsIntDispatchNegative isMinus t
  , Nat.IsNat t
  ) => IsIntDispatch Bool.True num
  where
    toIntDispatch _ _ = toNegativeInt (BProxy :: BProxy isMinus) (SProxy :: SProxy t)

instance isIntDispatchNoSign ::
  ( Nat.IsNat num
  ) => IsIntDispatch Bool.False num
  where
    toIntDispatch _ _ = Nat.toInt (SProxy :: SProxy num)

class IsIntDispatchNegative (isMinus :: Bool.Boolean) (num :: Symbol) where
  toNegativeInt :: BProxy isMinus -> SProxy num -> Int

instance isIntDispatchNegative ::
  ( Nat.IsNat num
  ) => IsIntDispatchNegative Bool.True num where
  toNegativeInt _ _ = negate (Nat.toInt (SProxy :: SProxy num))

instance isIntDispatchNotNegative ::
  ( Nat.IsNat num
  ) => IsIntDispatchNegative Bool.False num where
  toNegativeInt _ _ = Nat.toInt (SProxy :: SProxy num)

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

-- | IsNegativePred
class IsNegativePred (num :: Symbol) (b :: Bool.Boolean) | num -> b

instance isNegativeEmpty :: IsNegativePred "" Bool.False
else instance isNegativePredNonEmpty ::
  ( Normalize num num'
  , Symbol.Cons h t num' -- h t <- num'
  , IsMinusPred h isMinus
  , Nat.IsNatPred t isNat
  , Bool.And isMinus isNat b
  ) => IsNegativePred num b

class IsMinusPred (sign :: Symbol) (b :: Bool.Boolean) | sign -> b

instance isMinusPredMinus :: IsMinusPred "-" Bool.True
else instance isMinusOtherwise :: IsMinusPred sign Bool.False

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


-- | Compare
class Compare (x :: Symbol) (y :: Symbol) (o :: Ord.Ordering) | x y -> o

instance compareImpl ::
  ( IsInt x
  , IsInt y
  , Normalize x x'
  , Normalize y y'
  , IsNegativePred x' x_isNeg
  , IsNegativePred y' y_isNeg
  , CompareDispatch x_isNeg y_isNeg x' y' o
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

-- | Add
class Add (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z

instance addMatchSign ::
  ( IsInt x
  , IsInt y
  , Normalize x x'
  , Normalize y y'
  , IsNegativePred x' x_isNeg
  , IsNegativePred y' y_isNeg
  , AddDispatch x_isNeg y_isNeg x' y' z'
  , Normalize z' z
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
  , MinusNonNegative x_pos y_pos z -- x_pos y_pos -> z
  ) => AddDispatch Bool.False Bool.True x_pos y_neg z
else instance addNegPos :: -- (-x) + y = -(x - y)
  ( Inverse x_neg x_pos -- x_neg -> x_pos
  , MinusNonNegative y_pos x_pos z -- y_pos x_pos -> z_neg
  ) => AddDispatch Bool.True Bool.False x_neg y_pos z

-- | Minus
class Minus (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z

instance minusMatchSign ::
  ( IsInt x
  , IsInt y
  , Normalize x x'
  , Normalize y y'
  , IsNegativePred x' x_isNeg
  , IsNegativePred y' y_isNeg
  , MinusDispatch x_isNeg y_isNeg x' y' z'
  , Normalize z' z
  ) => Minus x y z

class MinusDispatch (x_isNeg :: Bool.Boolean) (y_isNeg :: Bool.Boolean) (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z

instance minusPosPos ::
  ( MinusNonNegative x_pos y_pos z
  ) => MinusDispatch Bool.False Bool.False x_pos y_pos z
else instance minusNegNeg :: -- (-x) - (-y) = -(x - y)
  ( Inverse x_neg x_pos
  , Inverse y_neg y_pos
  , MinusNonNegative x_pos y_pos z'
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
class MinusNonNegative (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z

instance minusNonNegativeImpl ::
  ( ReverseSymbol x x'
  , ReverseSymbol y y'
  , Compare x y o
  , MinusNonNegativeCompareDispatch o x' y' z'
  , ReverseSymbol z' z
  ) => MinusNonNegative x y z

class MinusNonNegativeCompareDispatch (o :: Ord.Ordering) (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z

instance minusNonNegativeCompareDispatchGT :: -- x > y
  -- x - y
  ( MinusNonNegativeReversed x y "0" z
  ) => MinusNonNegativeCompareDispatch Ord.GT x y z
else instance minusNonNegativeCompareDispatchLTorEQ :: -- x <= y
  -- x - y = -(y - x)
  ( MinusNonNegativeReversed y x "0" z'
  , ReverseSymbol z' z''
  , Inverse z'' z'''
  , ReverseSymbol z''' z
  ) => MinusNonNegativeCompareDispatch o x y z -- o == Ord.LT || o == Ord.GT

-- x >= y
class MinusNonNegativeReversed (x :: Symbol) (y :: Symbol) (carry :: Symbol) (z :: Symbol) | x y carry -> z

instance minusNonNegativeReversedBaseCase0 :: -- 0 - 0 = 0
  MinusNonNegativeReversed "" "" "0" ""
-- else instance minusNonNegativeReversedBaseCase1 :: -- 0 - y = -y
--   ( Symbol.Append y "-" z -- y "-" -> z
--   ) => MinusNonNegativeReversed "" y "0" z
else instance minusNonNegativeReversedBaseCase2 :: -- x - 0 = x
  MinusNonNegativeReversed x "" "0" x
-- else instance minusNonNegativeReversedBaseCase3 :: -- 0 - y + (-1) = -(y + 1)
--   ( Nat.Add y "1" y'
--   , Symbol.Append y' "-" z
--   ) => MinusNonNegativeReversed "" y "1" z
else instance minusNonNegativeReversedBaseCase4 :: -- 1 - 0 + (-1) = 0
  MinusNonNegativeReversed "1" "" "1" ""
else instance minusNonNegativeReversedBaseCase5 :: -- x - 0 + (-1) = x - 1
  ( MinusNonNegativeReversed x "1" "0" z -- x "1" -> "0" z
  ) => MinusNonNegativeReversed x "" "1" z
else instance minusNonNegativeReversedInductionStep ::
  ( Symbol.Cons x_h x_t x -- x_h x_t <- x
  , Symbol.Cons y_h y_t y -- y_h y_t <- y
  , Digit.Minus x_h y_h carry1 z_h1 -- x_h y_h -> carry1 z_h1
  , Digit.Minus z_h1 carry0 carry2 z_h -- z_h1 carry0 -> carry2 z_h
  , Digit.Minus carry1 carry2 "0" carry3 -- carry1 carry2 -> "0" carry3
  , MinusNonNegativeReversed x_t y_t carry3 z_t
  , Symbol.Append z_h z_t z
  ) => MinusNonNegativeReversed x y carry0 z

-- | Normalize

class Normalize (i :: Symbol) (o :: Symbol) | i -> o

instance normalizeImpl ::
  ( HasSignPred i hasSign
  , NormalizeDispatch hasSign i o
  ) => Normalize i o

class NormalizeDispatch (hasSign :: Bool.Boolean) (i :: Symbol) (o :: Symbol) | i -> o

instance normalizeHasSign ::
  ( Symbol.Cons sign num i -- sign num <- i
  , NormalizeRemoveZero num num_nozero
  , NormalizeSign sign num_nozero sign'
  , Symbol.Append sign' num_nozero o
  ) => NormalizeDispatch Bool.True i o
else instance normalizeNoSign ::
  ( NormalizeRemoveZero i o
  ) => NormalizeDispatch Bool.False i o

class NormalizeSign (sign :: Symbol) (num :: Symbol) (normalized :: Symbol) | sign -> normalized

instance normalizeSignPlus :: NormalizeSign "+" num ""
else instance normalizeSignMinus :: NormalizeSign "-" "0" ""
else instance normalizeSignMinus2 :: NormalizeSign "-" num "-"

class NormalizeRemoveZero (num :: Symbol) (normalized :: Symbol) | num -> normalized
instance normalizeRemoveZeroImpl ::
  ( NormalizeRemoveZeroBaseCase num o
  , NormalizeRemoveZeroRecoverZero o normalized
  ) => NormalizeRemoveZero num normalized

class NormalizeRemoveZeroBaseCase (num :: Symbol) (o :: Symbol) | num -> o
instance normalizeRemoveZeroBaseCaseEmpty ::
  NormalizeRemoveZeroBaseCase "" ""
else instance normalizeRemoveZeroBaseCaseNonEmpty ::
  ( Symbol.Cons h t num
  , NormalizeRemoveZeroInductionStep h t o
  ) => NormalizeRemoveZeroBaseCase num o

class NormalizeRemoveZeroInductionStep (h :: Symbol) (t :: Symbol) (o :: Symbol) | h t -> o

instance normalizeRemoveZeroInductionStepZero ::
  ( NormalizeRemoveZeroBaseCase t o
  ) => NormalizeRemoveZeroInductionStep "0" t o
else instance normalizeRemoveZeroInductionStepNonZero ::
  ( Symbol.Append h t o
  ) => NormalizeRemoveZeroInductionStep h t o

class NormalizeRemoveZeroRecoverZero (i :: Symbol) (o :: Symbol) | i -> o

instance normalizeRemoveZeroRecoverZeroEmpty ::
  NormalizeRemoveZeroRecoverZero "" "0"
else instance normalizeRemoveZeroRecoverZeroNonEmpty ::
  NormalizeRemoveZeroRecoverZero i i

-- | Pred
class Pred (num :: Symbol) (pred :: Symbol) | num -> pred

instance predAll ::
  ( IsInt num
  , Minus num "1" pred
  ) => Pred num pred


