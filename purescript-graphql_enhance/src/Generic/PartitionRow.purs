module Generic.PartitionRow where

import Prelude
import Data.Maybe (Maybe)
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Prim.Row as Row
import Type.Data.Boolean as Bool
import Generic.GraphQLType (Id)

class IsUnit a (b :: Bool.Boolean) | a -> b

instance isUnitYes :: IsUnit Unit Bool.True
else instance isUnitNo :: IsUnit a Bool.False

class IsScalar a (b :: Bool.Boolean) | a -> b

instance isScalarInt :: IsScalar Int Bool.True
else instance isScalarNumber :: IsScalar Number Bool.True
else instance isScalarString :: IsScalar String Bool.True
else instance isScalarId :: IsScalar Id Bool.True
else instance isScalarBoolean :: IsScalar Boolean Bool.True
else instance isScalarMaybeInt :: IsScalar (Maybe Int) Bool.True
else instance isScalarMaybeNumber :: IsScalar (Maybe Number) Bool.True
else instance isScalarMaybeString :: IsScalar (Maybe String) Bool.True
else instance isScalarMaybeId :: IsScalar (Maybe Id) Bool.True
else instance isScalarMaybeBoolean :: IsScalar (Maybe Boolean) Bool.True
else instance isScalarNo :: IsScalar a Bool.False

class PartitionRow (row :: # Type) (scalars :: # Type) (relations :: # Type) | row -> scalars relations

instance partitionRowToList ::
  ( RowList.RowToList row rl
  , PartitionRowList rl scalars relations
  ) => PartitionRow row scalars relations

class PartitionRowList (rl :: RowList) (scalars :: # Type) (relations :: # Type) | rl -> scalars relations

instance partitionRowListBaseCase ::
  PartitionRowList RowList.Nil () ()
else instance partitionRowListInductionStep ::
  ( IsScalar typ b -- typ -> b
  , PartitionRowListDispatch b name typ restRl scalars relations
  ) => PartitionRowList (RowList.Cons name typ restRl) scalars relations

class PartitionRowListDispatch (b :: Bool.Boolean) (name :: Symbol) typ (restRl :: RowList) (scalars :: # Type) (relations :: # Type) | b name typ restRl -> scalars relations

instance partitionRowListIsScalar ::
  ( PartitionRowList restRl restScalars relations
  , Row.Cons name typ restScalars scalars
  ) => PartitionRowListDispatch Bool.True name typ restRl scalars relations
else instance partitionRowListNotScalar ::
  ( PartitionRowList restRl scalars restRelations
  , Row.Cons name typ restRelations relations
  ) => PartitionRowListDispatch Bool.False name typ restRl scalars relations

