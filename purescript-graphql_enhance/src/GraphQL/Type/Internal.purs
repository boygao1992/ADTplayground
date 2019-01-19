module GraphQL.Type.Internal where

import Prelude

import Type.Row.Validation (class Validate, Required, Optional, Success)
import Type.Row.Utils as Row
import GraphQL.Data.FieldList as FieldList
import Data.Function.Uncurried (Fn1)
import Data.Generic.Rep (class Generic, Constructor, Argument)
import GraphQL.Type (ObjectType)
import Prim.RowList as RowList
import Type.Row as Row

foreign import _objectType :: forall objectRow a. Fn1 (Record objectRow) (ObjectType a)

{-
given = ctx
input = Record objectRow
Geneirc psType (Constructor psTypeName (Argument (Record psFnRow)))
RowToList psFnRow psFnRl
FieldList.FromRowList (psFnRl :: RowList) (psFnFl :: FieldList)
PartitionFieldList (psFnFl :: FieldList) (psScalarFnFl :: FieldList) (psRelationFnFl :: FieldList)
FieldList.RemoveArgs psScalarFnFl psScalarTypeFl
FieldList.ToRowList (psScalarTypeFl :: FieldList) (psScalarTypeRow :: # Type)
Validate ( name :: Required String, description :: Optional String) objectRow Success
FetchField "fields" objectRow (Record objectFieldsRow)
ValidateFields (psFl :: FieldList) (objectFieldsRow :: # Type) =
  FieldList.Cons (fieldName :: Symbol) (args :: # Type) (rela :: Relation) (typ :: Type) restPsFl psFl
  Row.Cons fieldName (Record fieldRow) restObjectFieldsRow objectFieldsRow
  IsScalarType (typ :: Type) (isScalar :: Bool.Boolean)
  if isScalar == Bool.True
  then
    RelationToPsType (rela :: Relation) (typ :: Type) (o :: Type)
    Validate (description :: Optional String) fieldRow
    FetchField "resolve" fieldRow (result :: FetchResult)
    if FetchFailure then pass
    if (FetchSuccess ((Record resolveInputRow) -> Aff o)) then
      Validate ( source :: Optional psScalarRow
               , args :: Required args
               , ctx :: Optional ctx
               )
               resolveInputRow
  else
    Generic a (Constructor name (Argument (Record typRow)))
    PartitionRow typRow typScalarRow typRelationRow
    RelationToPsType (rela :: Relation) (Record typeScalarRow) (o :: Type)
    Validate (description :: Optional String) fieldRow
    FetchField "resolve" fieldRow (result :: FetchResult)
    if FetchFailure then pass
    if (FetchSuccess ((Record resolveInputRow) -> Aff o)) then
      Validate ( source :: Optional psScalarRow
               , args :: Required args
               , ctx :: Optional ctx
               )
               resolveInputRow
-}

type ObjectTypeConstructor deps psType = Record deps -> ObjectType psType

class ToScalarTypeRow (psFnRow :: # Type) (psScalarTypeRow :: # Type) | psFnRow -> psScalarTypeRow

instance toScalarTypeRowImpl ::
  ( RowList.RowToList psFnRow psFnRl
  , FieldList.FromRowList psFnRl psFnFl
  , FieldList.PartitionFieldList psFnFl psScalarFnFl psRelationFnFl
  , FieldList.RemoveArgs psScalarFnFl psScalarTypeFl
  , FieldList.ToRowList psScalarTypeFl psScalarTypeRl
  , Row.ListToRow psScalarTypeRl psScalarTypeRow
  ) => ToScalarTypeRow psFnRow psScalarTypeRow

objectType
  :: forall objectRow deps psType psTypeName psFnRow psScalarTypeRow
     objectFieldsRow
   . Generic psType (Constructor psTypeName (Argument (Record psFnRow)))
  => ToScalarTypeRow psFnRow psScalarTypeRow
  => Validate (description :: Optional String) objectRow Success -- TODO direct error message instead of Failure data type
  => Row.FetchField "fields" objectRow (Row.FetchSuccess (Record objectFieldsRow)) -- TODO direct error message instead of FetchFailure data type
  => Record objectRow
  -> ObjectTypeConstructor deps psType
  -> Unit
objectType _ _ = unit

--   => Record objectRow
--   -> ObjectTypeConstructor deps psType
