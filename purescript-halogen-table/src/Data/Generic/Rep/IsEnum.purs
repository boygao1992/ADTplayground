module Data.Generic.Rep.IsEnum where

import Data.Generic.Rep (Constructor, NoArguments, Sum)
import Type.Data.Boolean as Bool
import Prim.TypeError

class IsEnum rep

instance isEnumBaseCase ::
  IsEnum (Sum (Constructor name1 NoArguments) (Constructor name2 NoArguments))
else
instance isEnumInductionStep ::
  ( IsEnum r
  ) => IsEnum (Sum (Constructor name NoArguments) r)
else
instance isEnumError ::
  ( Fail
    ( Text "not a valid Enum"
    )
  ) => IsEnum otherwise

class IsEnumPred rep (b :: Bool.Boolean) | rep -> b

instance isEnumPredBaseCase ::
  IsEnumPred (Sum (Constructor name1 NoArguments) (Constructor name2 NoArguments)) Bool.True
else instance isEnumPredInductionStep ::
  ( IsEnumPred r b
  ) => IsEnumPred (Sum (Constructor name NoArguments) r) b
else instance isEnumPredFail ::
  IsEnumPred rep Bool.False
