module GraphQL.Type.Internal.NullableToMaybe where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import GraphQL.Type.Internal (Id, class IsListPred)
import Type.Data.Boolean (BProxy(..))
import Type.Data.Boolean as Bool
import Type.Row.Utils (class IsRecordPred)
import Prim.TypeError (class Fail, Above, Quote, Text)
import Type.Data.RowList (RLProxy(..))
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Prim.Row as Row
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Data.Symbol (SProxy(..))
import Type.Data.Symbol as Symbol

-- NOTE different from GraphQL.Type.Internal (class IsScalarPred)
class IsScalarPred a (b :: Bool.Boolean) | a -> b
instance isScalarPredInt ::
  IsScalarPred Int Bool.True
else instance isScalarPredNumber ::
  IsScalarPred Number Bool.True
else instance isScalarPredString ::
  IsScalarPred String Bool.True
else instance isScalarPredId ::
  IsScalarPred Id Bool.True
else instance isScalarPredBoolean ::
  IsScalarPred Boolean Bool.True
else instance isScalarPredNo ::
  IsScalarPred other Bool.False

class IsMaybePred a (b :: Bool.Boolean) | a -> b
instance isMaybePredYes :: IsMaybePred (Maybe a) Bool.True
else instance isMaybePredNo :: IsMaybePred other Bool.False

class IsNullablePred a (b :: Bool.Boolean) | a -> b
instance isNullablePredYes :: IsNullablePred (Nullable a) Bool.True
else instance isNullablePredNo :: IsNullablePred other Bool.False

-- NOTE for args from InputObject, thus leaf nodes are all scalars
class NullableToMaybe i o | i -> o where
  nullableToMaybe :: i -> o
  -- maybeToNullable :: o -> i

instance nullableToMaybeTypeDispatch ::
  ( IsScalarPred i isScalar
  , IsListPred i isList
  , IsRecordPred i isRecord
  , IsNullablePred i isNullable
  , NullableToMaybeDispatch isScalar isList isRecord isNullable i o
  ) => NullableToMaybe i o
  where
    nullableToMaybe i =
      nullableToMaybeDispatch
        (BProxy :: BProxy isScalar)
        (BProxy :: BProxy isList)
        (BProxy :: BProxy isRecord)
        (BProxy :: BProxy isNullable)
        i

class NullableToMaybeDispatch
  (isScalar :: Bool.Boolean) (isList :: Bool.Boolean)
  (isRecord :: Bool.Boolean) (isNullable :: Bool.Boolean)
  i o
  | isScalar isList isRecord isNullable i -> o
  where
    nullableToMaybeDispatch
      :: BProxy isScalar -> BProxy isList
         -> BProxy isRecord -> BProxy isNullable -> i
      -> o

instance nullableToMaybeDispatchIsScalar ::
  NullableToMaybeDispatch Bool.True Bool.False Bool.False Bool.False i i
  where
    nullableToMaybeDispatch _ _ _ _ i = i
else instance nullableToMaybeDispatchIsList ::
  ( Functor f
  , NullableToMaybe a restO
  ) => NullableToMaybeDispatch Bool.False Bool.True Bool.False Bool.False (f a) (f restO)
  where
    nullableToMaybeDispatch _ _ _ _ i = nullableToMaybe <$> i
else instance nullableToMaybeDispatchIsRecord ::
  ( NullableToMaybeIsRecord row o
  ) => NullableToMaybeDispatch Bool.False Bool.False Bool.True Bool.False (Record row) (Record o)
  where
    nullableToMaybeDispatch _ _ _ _ i = nullableToMaybeIsRecord i
else instance nullableToMaybeDispatchIsNullable ::
  -- NOTE Nullable doesn't have a Functor instance so it blocks further conversion
  -- TODO try Data.Nullable.Safe
  NullableToMaybeDispatch Bool.False Bool.False Bool.False Bool.True (Nullable a) (Maybe a)
  where
    nullableToMaybeDispatch _ _ _ _ i = toMaybe i
else instance nullableToMaybeDispatchInvalid ::
  Fail
  ( Above
    ( Text "NullableToMaybeDispatch: Invalid type")
    ( Quote i)
  )
  => NullableToMaybeDispatch isScalar isList isRecord isNullable i o
  where
    nullableToMaybeDispatch = nullableToMaybeDispatch

class NullableToMaybeIsRecord (i :: # Type) (o :: # Type) | i -> o
  where
    nullableToMaybeIsRecord :: Record i -> Record o

instance nullableToMaybeIsRecordToRowList ::
  ( RowList.RowToList i rl
  , NullableToMaybeIsRecordRowList rl i o
  ) => NullableToMaybeIsRecord i o
  where
    nullableToMaybeIsRecord x
      = Builder.build
        ( nullableToMaybeIsRecordRowList
            (RLProxy :: RLProxy rl)
            x
        )
        x

class NullableToMaybeIsRecordRowList (rl :: RowList) (i :: # Type) (o :: # Type)
  | rl i -> o
  where
    nullableToMaybeIsRecordRowList :: RLProxy rl -> Record i -> Builder (Record i) (Record o)

instance nullableToMaybeIsRecordRowListNil ::
  NullableToMaybeIsRecordRowList RowList.Nil i i
  where
    nullableToMaybeIsRecordRowList _ _ = identity
else instance nullableToMaybeIsRecordRowListCons ::
  ( NullableToMaybeIsRecordRowList restRl i o'
  , Symbol.IsSymbol name
  , NullableToMaybe a b
  , Row.Cons name a restO o'
  , Row.Cons name b restO o
  ) => NullableToMaybeIsRecordRowList (RowList.Cons name a restRl) i o
  where
    nullableToMaybeIsRecordRowList _ x
        = Builder.modify
            (SProxy :: SProxy name)
            (nullableToMaybe :: a -> b)
      <<< nullableToMaybeIsRecordRowList
            (RLProxy :: RLProxy restRl)
            x
