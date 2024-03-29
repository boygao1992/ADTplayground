module Deprecated.FieldList where

import GraphQL.Type.Data.Relation (class ParseRelation, kind Relation)
import GraphQL.Type.Internal (class IsScalarPred)
import Prim.RowList (kind RowList)
import Type.Row as Row
import Prim.RowList as RowList
import Type.Data.Boolean as Bool

foreign import kind Arguments
foreign import data Args :: # Type -> Arguments
foreign import data NoArgs :: Arguments


foreign import kind FieldList -- Field List
foreign import data Nil :: FieldList
-- (name :: Symbol) (args :: # Type) (rela :: Relation) (a :: Type)
foreign import data Cons :: Symbol -> Arguments -> Relation -> Type -> FieldList -> FieldList

-- | FromRowList (isomorphic, bidirectional)
class FromRowList (rl :: RowList) (fl :: FieldList) | rl -> fl, fl -> rl

instance fromRowListBaseCase ::
  FromRowList RowList.Nil Nil
else instance fromRowListInductionStepArgs ::
  ( FromRowList restRl restFl
  , ParseRelation typ relation a -- typ -> relation a
  ) => FromRowList (RowList.Cons name (Record args -> typ) restRl) (Cons name (Args args) relation a restFl)
else instance fromRowListIndcutionStepNoArgs ::
  ( FromRowList restRl restFl
  , ParseRelation typ relation a
  ) => FromRowList (RowList.Cons name typ restRl) (Cons name NoArgs relation a restFl)

-- -- | FromRow
class FromRow (row :: # Type) (fl :: FieldList) | row -> fl

instance fromRowImpl ::
  ( RowList.RowToList row rl
  , FromRowList rl fl -- rl -> fl
  ) => FromRow row fl

-- -- | ToRow
class ToRow (fl :: FieldList) (row :: # Type) | fl -> row

instance toRowImpl ::
  ( FromRowList rl fl -- rl <- fl
  , Row.ListToRow rl row
  ) => ToRow fl row

-- | PartitionFieldList
class PartitionFieldList (fl :: FieldList) (scalarFl :: FieldList) (relationFl :: FieldList) | fl -> scalarFl relationFl

instance partitionFieldListBaseCase ::
  PartitionFieldList Nil Nil Nil
else instance parititionFieldListIsScalarDispatch ::
  ( IsScalarPred a isScalar
  , PartitionFieldListDispatch isScalar name args rela a restFl scalarFl relationFl
  ) => PartitionFieldList (Cons name args rela a restFl) scalarFl relationFl

class PartitionFieldListDispatch (isScalar :: Bool.Boolean) (name :: Symbol) (args :: Arguments) (rela :: Relation) a (restFl :: FieldList) (scalarFl :: FieldList) (relationFl :: FieldList) | isScalar name args rela a restFl -> scalarFl relationFl

instance partitionFieldListIsScalar ::
  ( PartitionFieldList restFl restScalarFl relationFl
  ) => PartitionFieldListDispatch Bool.True name args rela a restFl (Cons name args rela a restScalarFl) relationFl
else instance partitionFieldListNotScalar ::
  ( PartitionFieldList restFl scalarFl restRelationFl
  ) => PartitionFieldListDispatch Bool.False name args rela a restFl scalarFl (Cons name args rela a restRelationFl)

-- | RemoveArgs
class RemoveArgs (i :: FieldList) (o :: FieldList) | i -> o

instance removeArgsBaseCase ::
  RemoveArgs Nil Nil
else instance removeArgsInductionStepArgs ::
  ( RemoveArgs restI restO
  ) => RemoveArgs (Cons name (Args args) rela a restI) (Cons name NoArgs rela a restO)
else instance removeArgsInductionStepNoArgs ::
  ( RemoveArgs restI restO
  ) => RemoveArgs (Cons name NoArgs rela a restI) (Cons name NoArgs rela a restO)


-- | ToScalarTypeRow
class ToScalarTypeRow (fnFl :: FieldList) (scalarTypeRow :: # Type) | fnFl -> scalarTypeRow

instance toScalarTypeRowImpl ::
  ( PartitionFieldList fnFl scalarFnFl relationFnFl
  , RemoveArgs scalarFnFl scalarTypeFl
  , ToRow scalarTypeFl scalarTypeRow
  ) => ToScalarTypeRow fnFl scalarTypeRow
