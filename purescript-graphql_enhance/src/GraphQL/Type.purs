module GraphQL.Type where

import Prelude

import Data.NonEmpty (NonEmpty)
import Data.Maybe (Maybe)
import Type.Data.Boolean as Bool

data GraphQLObject psType

newtype Id = Id String

-- | IsUnitPred
class IsUnitPred a (b :: Bool.Boolean) | a -> b

instance isUnitPredYes :: IsUnitPred Unit Bool.True
else instance isUnitPredNo :: IsUnitPred a Bool.False

-- | IsScalarPred
class IsScalarPred a (b :: Bool.Boolean) | a -> b

instance isScalarPredInt :: IsScalarPred Int Bool.True
else instance isScalarPredNumber :: IsScalarPred Number Bool.True
else instance isScalarPredString :: IsScalarPred String Bool.True
else instance isScalarPredId :: IsScalarPred Id Bool.True
else instance isScalarPredBoolean :: IsScalarPred Boolean Bool.True
else instance isScalarPredNo :: IsScalarPred a Bool.False

{- mapping
Maybe a -> a
a -> NotNull a
Array a -> NotNull List (NotNull a)
-}

foreign import kind Relation
foreign import data ExactOne :: Relation -- a
foreign import data ZeroOrOne :: Relation -- Maybe a
foreign import data ZeroOrMore :: Relation -- Array a
foreign import data OneOrMore :: Relation -- NonEmpty Array a

-- | ParseRelation
class ParseRelation typ (rela :: Relation) a

instance parseTypeOneOrMore :: ParseRelation (NonEmpty Array a) OneOrMore a
else instance parseTypeZeroOrMore :: ParseRelation (Array a) ZeroOrMore a
else instance parseTypeZeroOrOne :: ParseRelation (Maybe a) ZeroOrOne a
else instance parseTypeExactOne :: ParseRelation a ExactOne a
{-
given = ctx
input = Record objectRow
Geneirc psType (Constructor typeName (Argument (Record psTypeRow)))
RowListToFieldList (psTypeRow :: # Type) (psFl :: FieldList)
PartitionFieldList (psFl :: FieldList) (psScalarFl :: FieldList) (psRelationFl :: FieldList)
FieldListToRow (psScalarFl :: FieldList) (psScalarRow :: # Type)
Validate ( name :: Required String, description :: Optional String) objectRow Success
FetchField "fields" objectRow (Record objectFieldsRow)
ValidateFields (psFl :: FieldList) (objectFieldsRow :: # Type)
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
