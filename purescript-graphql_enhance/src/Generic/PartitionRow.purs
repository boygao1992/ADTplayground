module Generic.PartitionRow where

import Prelude
import Data.Maybe (Maybe)
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Prim.Row as Row
import Type.Data.Boolean as Bool

newtype Id = Id String

class IsUnitPred a (b :: Bool.Boolean) | a -> b

instance isUnitPredYes :: IsUnitPred Unit Bool.True
else instance isUnitPredNo :: IsUnitPred a Bool.False

class IsScalarPred a (b :: Bool.Boolean) | a -> b

instance isScalarPredInt :: IsScalarPred Int Bool.True
else instance isScalarPredNumber :: IsScalarPred Number Bool.True
else instance isScalarPredString :: IsScalarPred String Bool.True
else instance isScalarPredId :: IsScalarPred Id Bool.True
else instance isScalarPredBoolean :: IsScalarPred Boolean Bool.True
else instance isScalarPredMaybeInt :: IsScalarPred (Maybe Int) Bool.True
else instance isScalarPredMaybeNumber :: IsScalarPred (Maybe Number) Bool.True
else instance isScalarPredMaybeString :: IsScalarPred (Maybe String) Bool.True
else instance isScalarPredMaybeId :: IsScalarPred (Maybe Id) Bool.True
else instance isScalarPredMaybeBoolean :: IsScalarPred (Maybe Boolean) Bool.True
else instance isScalarPredNo :: IsScalarPred a Bool.False

class PartitionRow (row :: # Type) (scalars :: # Type) (relations :: # Type) | row -> scalars relations

instance partitionRowToList ::
  ( RowList.RowToList row rl
  , PartitionRowList rl scalars relations
  ) => PartitionRow row scalars relations

class PartitionRowList (rl :: RowList) (scalars :: # Type) (relations :: # Type) | rl -> scalars relations

instance partitionRowListBaseCase ::
  PartitionRowList RowList.Nil () ()
else instance partitionRowListInductionStep ::
  ( IsScalarPred typ b -- typ -> b
  , PartitionRowListDispatch b name typ restRl scalars relations
  ) => PartitionRowList (RowList.Cons name typ restRl) scalars relations

class PartitionRowListDispatch (b :: Bool.Boolean) (name :: Symbol) typ (restRl :: RowList) (scalars :: # Type) (relations :: # Type) | b name typ restRl -> scalars relations

instance partitionRowListIsScalarPred ::
  ( PartitionRowList restRl restScalars relations
  , Row.Cons name typ restScalars scalars
  ) => PartitionRowListDispatch Bool.True name typ restRl scalars relations
else instance partitionRowListNotScalar ::
  ( PartitionRowList restRl scalars restRelations
  , Row.Cons name typ restRelations relations
  ) => PartitionRowListDispatch Bool.False name typ restRl scalars relations

