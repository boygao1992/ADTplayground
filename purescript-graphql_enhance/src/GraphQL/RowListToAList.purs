module GraphQL.RowListToFieldList where

import GraphQL.Type (class IsScalarPred, class ParseRelation, kind Relation)
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Type.Data.Boolean as Bool

foreign import kind FieldList -- Field List
foreign import data Nil :: FieldList
-- (name :: Symbol) (args :: # Type) (rela :: Relation) (a :: Type)
foreign import data Cons :: Symbol -> # Type -> Relation -> Type -> FieldList -> FieldList


-- | RowListToFieldList
class RowListToFieldList (rl :: RowList) (fl :: FieldList) | rl -> fl

instance rowListToFieldListBaseCase ::
  RowListToFieldList RowList.Nil Nil
else instance rowListToFieldListInductionStepArgs ::
  ( RowListToFieldList restRl restAl
  , ParseRelation typ relation a
  ) => RowListToFieldList (RowList.Cons name (Record row -> typ) restRl) (Cons name row relation a restAl)
else instance rowListToFieldListIndcutionStepNoArgs ::
  ( RowListToFieldList restRl restAl
  , ParseRelation typ relation a
  ) => RowListToFieldList (RowList.Cons name typ restRl) (Cons name () relation a restAl)

-- | PartitionFieldList
class PartitionFieldList (fl :: FieldList) (scalarFl :: FieldList) (relationFl :: FieldList) | fl -> scalarFl relationFl

instance partitionFieldListBaseCase ::
  PartitionFieldList Nil Nil Nil
else instance parititionFieldListIsScalarDispatch ::
  ( IsScalarPred a isScalar
  , PartitionFieldListDispatch isScalar name args rela a restAl scalarFl relationFl
  ) => PartitionFieldList (Cons name args rela a restAl) scalarFl relationFl

class PartitionFieldListDispatch (isScalar :: Bool.Boolean) (name :: Symbol) (args :: # Type) (rela :: Relation) a (restAl :: FieldList) (scalarFl :: FieldList) (relationFl :: FieldList) | isScalar name args rela a restAl -> scalarFl relationFl

instance partitionFieldListIsScalar ::
  ( PartitionFieldList restAl restScalarFl relationFl
  ) => PartitionFieldListDispatch Bool.True name args rela a restAl (Cons name args rela a restScalarFl) relationFl
else instance partitionFieldListNotScalar ::
  ( PartitionFieldList restAl scalarFl restRelationFl
  ) => PartitionFieldListDispatch Bool.True name args rela a restAl scalarFl (Cons name args rela a restRelationFl)

