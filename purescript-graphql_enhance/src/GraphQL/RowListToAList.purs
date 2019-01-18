module GraphQL.RowListToAList where

import GraphQL.Type (class IsScalarPred, class ParseType, kind Relation)
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Type.Data.Boolean as Bool

foreign import kind AList
foreign import data Nil :: AList
-- (name :: Symbol) (args :: # Type) (rela :: Relation) (a :: Type)
foreign import data Cons :: Symbol -> # Type -> Relation -> Type -> AList -> AList


-- | RowListToAList
class RowListToAList (rl :: RowList) (al :: AList) | rl -> al

instance rowListToAListBaseCase ::
  RowListToAList RowList.Nil Nil
else instance rowListToAListInductionStepArgs ::
  ( RowListToAList restRl restAl
  , ParseType typ relation a
  ) => RowListToAList (RowList.Cons name (Record row -> typ) restRl) (Cons name row relation a restAl)
else instance rowListToAListIndcutionStepNoArgs ::
  ( RowListToAList restRl restAl
  , ParseType typ relation a
  ) => RowListToAList (RowList.Cons name typ restRl) (Cons name () relation a restAl)

-- | PartitionAList
class PartitionAList (al :: AList) (scalarAl :: AList) (relationAl :: AList) | al -> scalarAl relationAl

instance partitionAListBaseCase ::
  PartitionAList Nil Nil Nil
else instance parititionAListIsScalarDispatch ::
  ( IsScalarPred a isScalar
  , PartitionAListDispatch isScalar name args rela a restAl scalarAl relationAl
  ) => PartitionAList (Cons name args rela a restAl) scalarAl relationAl

class PartitionAListDispatch (isScalar :: Bool.Boolean) (name :: Symbol) (args :: # Type) (rela :: Relation) a (restAl :: AList) (scalarAl :: AList) (relationAl :: AList) | isScalar name args rela a restAl -> scalarAl relationAl

instance partitionAListIsScalar ::
  ( PartitionAList restAl restScalarAl relationAl
  ) => PartitionAListDispatch Bool.True name args rela a restAl (Cons name args rela a restScalarAl) relationAl
else instance partitionAListNotScalar ::
  ( PartitionAList restAl scalarAl restRelationAl
  ) => PartitionAListDispatch Bool.True name args rela a restAl scalarAl (Cons name args rela a restRelationAl)
