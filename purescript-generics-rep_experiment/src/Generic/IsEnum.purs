module Generic.IsEnum where

import Data.Generic.Rep (Constructor, NoArguments, Sum)
import Type.Data.Boolean as Bool

class IsEnum rep

instance isEnumBaseCase ::
  IsEnum (Sum (Constructor name1 NoArguments) (Constructor name2 NoArguments))
else instance isEnumInductionStep ::
  ( IsEnum r
  ) => IsEnum (Sum (Constructor name NoArguments) r)

class IsEnumPred rep (b :: Bool.Boolean) | rep -> b

instance isEnumPredBaseCase ::
  IsEnumPred (Sum (Constructor name1 NoArguments) (Constructor name2 NoArguments)) Bool.True
else instance isEnumPredInductionStep ::
  ( IsEnumPred r b
  ) => IsEnumPred (Sum (Constructor name NoArguments) r) b
else instance isEnumPredFail ::
  IsEnumPred rep Bool.False

