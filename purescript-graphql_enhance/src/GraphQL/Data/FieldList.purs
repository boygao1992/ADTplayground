module GraphQL.Data.FieldList where

import GraphQL.Type (class IsScalarPred, class ParseRelation, kind Relation)
import Prim.RowList (kind RowList)
import Type.Row as Row
import Prim.RowList as RowList
import Type.Data.Boolean as Bool

foreign import kind FieldList -- Field List
foreign import data Nil :: FieldList
-- (name :: Symbol) (args :: # Type) (rela :: Relation) (a :: Type)
foreign import data Cons :: Symbol -> # Type -> Relation -> Type -> FieldList -> FieldList

-- | FromRowList
class FromRowList (rl :: RowList) (fl :: FieldList) | rl -> fl

instance fromRowListBaseCase ::
  FromRowList RowList.Nil Nil
else instance fromRowListInductionStepArgs ::
  ( FromRowList restRl restFl
  , ParseRelation typ relation a -- typ -> relation a
  ) => FromRowList (RowList.Cons name (Record args -> typ) restRl) (Cons name args relation a restFl)
else instance fromRowListIndcutionStepNoArgs ::
  ( FromRowList restRl restFl
  , ParseRelation typ relation a
  ) => FromRowList (RowList.Cons name typ restRl) (Cons name () relation a restFl)

-- | FromRow
class FromRow (row :: # Type) (fl :: FieldList) | row -> fl

instance fromRowImpl ::
  ( RowList.RowToList row rl
  , FromRowList rl fl
  ) => FromRow row fl

-- | ToRowList
class ToRowList (fl :: FieldList) (rl :: RowList) | fl -> rl

instance toRowListBaseCase ::
  ToRowList Nil RowList.Nil
else instance toRowListInductionStepArgsRl ::
  ( RowList.RowToList args argsRl
  , ParseRelation typ relation a -- typ <- relation a
  , ToRowListDispatch argsRl name args typ restFl rl
  ) => ToRowList (Cons name args relation a restFl) rl

class ToRowListDispatch (argsRl :: RowList) (name :: Symbol) (args :: # Type) typ (restFl :: FieldList) (rl :: RowList) | argsRl name args typ restFl -> rl

instance toRowListNoArgs ::
  ( ToRowList restFl restRl
  ) => ToRowListDispatch RowList.Nil name args typ restFl (RowList.Cons name typ restRl)
else instance toRowListArgs ::
  ( ToRowList restFl restRl
  ) => ToRowListDispatch argsRl name args typ restFl (RowList.Cons name (Record args -> typ) restRl)

-- | ToRow
class ToRow (fl :: FieldList) (row :: # Type) | fl -> row

instance toRowImpl ::
  ( ToRowList fl rl
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

class PartitionFieldListDispatch (isScalar :: Bool.Boolean) (name :: Symbol) (args :: # Type) (rela :: Relation) a (restFl :: FieldList) (scalarFl :: FieldList) (relationFl :: FieldList) | isScalar name args rela a restFl -> scalarFl relationFl

instance partitionFieldListIsScalar ::
  ( PartitionFieldList restFl restScalarFl relationFl
  ) => PartitionFieldListDispatch Bool.True name args rela a restFl (Cons name args rela a restScalarFl) relationFl
else instance partitionFieldListNotScalar ::
  ( PartitionFieldList restFl scalarFl restRelationFl
  ) => PartitionFieldListDispatch Bool.True name args rela a restFl scalarFl (Cons name args rela a restRelationFl)

-- | RemoveArgs
class RemoveArgs (i :: FieldList) (o :: FieldList) | i -> o

instance removeArgsBaseCase ::
  RemoveArgs Nil Nil
else instance removeArgsInductionStep ::
  ( RemoveArgs restI restO
  ) => RemoveArgs (Cons name args rela a restI) (Cons name () rela a restO)

