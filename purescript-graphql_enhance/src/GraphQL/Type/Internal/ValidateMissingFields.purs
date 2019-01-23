module GraphQL.Type.Internal.ValidateMissingFields where

-- TODO fields in source are all scalars
-- TODO add Context (ctx)

import Prelude

import Data.Generic.Rep (class Generic, Argument, Constructor)
import Effect.Aff (Aff)
import GraphQL.Type.Data.FieldList as FieldList
import GraphQL.Type.Data.Relation (class ParseRelation, kind Relation)
import GraphQL.Type.Internal (class IsScalarPred, GraphQLType)
import Prim.TypeError (class Fail, Text, Quote, Above, Beside)
import Type.Data.Boolean as Bool
import Type.Proxy (Proxy(..))
import Type.Row (class Cons) as Row
import Type.Row.Utils (class HasFieldPred, class FetchField, FetchFailure, FetchSuccess, kind FetchResult) as Row
import Type.Row.Validation (class ValidateExclusive, Optional, OptionalField, RequiredField)

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

type ObjectTypeConstructor deps psType = Record deps -> GraphQLType psType

-- | ToScalarTypeRow
class GenericToScalarTypeRow a (scalarTypeRow :: # Type) | a -> scalarTypeRow

instance genericToScalarTypeRowImpl ::
  ( Generic a (Constructor name (Argument (Record fnRow)))
  , FieldList.FromRow fnRow fnFl
  , FieldList.ToScalarTypeRow fnFl scalarTypeRow
  ) => GenericToScalarTypeRow a scalarTypeRow

-- | ValidateObjectFields
class ValidateObjectFields (source :: # Type) (psFnFl :: FieldList.FieldList) (objectFieldsRow :: # Type)

instance validateObjectFieldsNil ::
  ValidateObjectFields source FieldList.Nil objectFieldsRow
instance validateObjectFieldsConsDispatch ::
  ( IsScalarPred a isScalar
  , Row.HasFieldPred objectFieldsRow name hasField
  , ValidateObjectFieldFirstDispatch hasField isScalar name args rela a source objectFieldsRow
  , ValidateObjectFields source restPsFnFl objectFieldsRow
  ) => ValidateObjectFields source (FieldList.Cons name args rela a restPsFnFl) objectFieldsRow

class ValidateObjectFieldFirstDispatch (hasField :: Bool.Boolean) (isScalar :: Bool.Boolean) (name :: Symbol) (args :: FieldList.Arguments) (rela :: Relation) a  (source :: # Type) (objectFieldsRow :: # Type)

instance validateObjectFieldFirstDispatchNoFieldIsScalar ::
  -- NOTE Scalar field declaration is optional
  -- because both its description and resolver are optional
  ValidateObjectFieldFirstDispatch Bool.False Bool.True name args rela a source objectFieldsRow
else instance validateObjectFieldFirstDispatchNoFieldNotScalar ::
  Fail
  ( Beside
    ( Text "Missing declaration of Relational field `")
    ( Beside
      ( Text name)
      ( Text "`.")
    )
  )
  => ValidateObjectFieldFirstDispatch Bool.False Bool.False name args rela a source objectFieldsRow
else instance validateObjectFieldFirstDispatchHasField ::
  ( Row.Cons name (Record objectFieldRow) restObjectFieldsRow objectFieldsRow
  , ValidateObjectFieldSecondDispatch isScalar name args rela a source objectFieldRow
  ) => ValidateObjectFieldFirstDispatch Bool.True isScalar name args rela a source objectFieldsRow


-- | ValidateObjectFieldSecondDispatch
class ValidateObjectFieldSecondDispatch (isScalar :: Bool.Boolean) (name :: Symbol) (args :: FieldList.Arguments) (rela :: Relation) a  (source :: # Type) (objectFieldRow :: # Type)

instance validateObjectFieldIsScalarNoArgs ::
  ( ParseRelation typ rela a -- typ <- rela a
  , ValidateExclusive
    ( description :: Optional String
    , resolve :: OptionalField
    ) objectFieldRow
  , Row.FetchField "resolve" objectFieldRow resolveFn
  , ValidateObjectFieldSecondDispatchNoArgs resolveFn source typ
  ) => ValidateObjectFieldSecondDispatch Bool.True name FieldList.NoArgs rela a source objectFieldRow
else instance validateObjectFieldIsScalarWithArgs ::
  ( ParseRelation typ rela a -- typ <- rela a
  , ValidateExclusive
    ( description :: Optional String
    , resolve :: RequiredField
    ) objectFieldRow
  , Row.FetchField "resolve" objectFieldRow resolveFn
  , ValidateObjectFieldSecondDispatchWithArgs name resolveFn args source typ
  ) => ValidateObjectFieldSecondDispatch Bool.True name (FieldList.Args args) rela a source objectFieldRow
else instance validateObjectFieldNotScalarNoArgs ::
  ( GenericToScalarTypeRow a aScalarRow
  , ParseRelation typ rela (Record aScalarRow) -- typ <- rela (Record aScalarRow)
  , ValidateExclusive
    ( description :: Optional String
    , resolve :: RequiredField
    ) objectFieldRow
  , Row.FetchField "resolve" objectFieldRow resolveFn
  , ValidateObjectFieldSecondDispatchNoArgs resolveFn source typ
  ) => ValidateObjectFieldSecondDispatch Bool.False name FieldList.NoArgs rela a source objectFieldRow
else instance validateObjectFieldNotScalarWithArgs ::
  ( GenericToScalarTypeRow a aScalarRow
  , ParseRelation typ rela (Record aScalarRow) -- typ <- rela (Record aScalarRow)
  , ValidateExclusive
    ( description :: Optional String
    , resolve :: RequiredField
    ) objectFieldRow
  , Row.FetchField "resolve" objectFieldRow resolveFn
  , ValidateObjectFieldSecondDispatchWithArgs name resolveFn args source typ
  ) => ValidateObjectFieldSecondDispatch Bool.False name (FieldList.Args args) rela a source objectFieldRow

class ValidateObjectFieldSecondDispatchNoArgs (resolveFn :: Row.FetchResult) (source :: # Type) typ

instance validateObjectFieldNoArgsNoResolve ::
  ValidateObjectFieldSecondDispatchNoArgs Row.FetchFailure source typ
else instance validateObjectFieldNoArgsWithResolve ::
  ( ValidateObjectFieldSecondDispatchNoArgsSuccess resolve source typ
  ) => ValidateObjectFieldSecondDispatchNoArgs (Row.FetchSuccess (resolve) rest) source typ

class ValidateObjectFieldSecondDispatchNoArgsSuccess (resolve :: Type) (source :: # Type) typ | resolve -> source typ, source typ -> resolve

instance validateObjectFieldNoArgsSuccess ::
  ( Row.Cons "source" (Record source) () i
  ) => ValidateObjectFieldSecondDispatchNoArgsSuccess (Record i -> Aff typ) source typ

class ValidateObjectFieldSecondDispatchWithArgs (name :: Symbol) (resolveFn :: Row.FetchResult) (args :: # Type) (source :: # Type) typ

instance validateObjectFieldWithArgsNoResolve ::
  Fail
  ( Above
    (Beside (Text "Missing resolve function for field `") (Beside (Text name) (Text "`:")))
    (Quote ({source :: Record source, args :: Record args} -> Aff typ))
  )
  => ValidateObjectFieldSecondDispatchWithArgs name Row.FetchFailure args source typ
else instance validateObjectFieldWithArgsWithResolve ::
  ( ValidateObjectFieldSecondDispatchWithArgsSuccess resolve args source typ
  ) => ValidateObjectFieldSecondDispatchWithArgs name (Row.FetchSuccess resolve rest) args source typ

class ValidateObjectFieldSecondDispatchWithArgsSuccess (resolve :: Type) (args :: # Type) (source :: # Type) typ | resolve -> args source typ, args source typ -> resolve

instance validateObjectFieldWithArgsSuccess ::
  -- Fail
  -- ( Quote typ)
  -- => ValidateObjectFieldSecondDispatchWithArgsSuccess (Record i -> Aff tye) args source typ
  ( Row.Cons "source" (Record source) () i0
  , Row.Cons "args" (Record args) i0 i
  ) => ValidateObjectFieldSecondDispatchWithArgsSuccess (Record i -> Aff typ) args source typ



-- | objectType

objectType
  :: forall objectRow deps psType psTypeName psFnRow psFnFl psScalarTypeRow
     objectFieldsRow
     rest
   . Generic psType (Constructor psTypeName (Argument (Record psFnRow)))
  => FieldList.FromRow psFnRow psFnFl
  => FieldList.ToScalarTypeRow psFnFl psScalarTypeRow -- psScalarTypeRow ~ source
  => ValidateExclusive
     ( description :: Optional String
     , fields :: RequiredField
     ) objectRow
  => Row.FetchField "fields" objectRow (Row.FetchSuccess (Record objectFieldsRow) rest)
  => ValidateObjectFields psScalarTypeRow psFnFl objectFieldsRow
  => Record objectRow
  -> ObjectTypeConstructor deps psType
  -> Unit -- TODO class InjectName, class InjectType, class ToGraphQLType
objectType _ _ = unit

--   => Record objectRow
--   -> ObjectTypeConstructor deps psType

-- Test

objectTypeTest
  :: forall objectRow psType psTypeName psFnRow psFnFl psScalarTypeRow
     objectFieldsRow
     rest
   . Generic psType (Constructor psTypeName (Argument (Record psFnRow)))
  => FieldList.FromRow psFnRow psFnFl
  => FieldList.ToScalarTypeRow psFnFl psScalarTypeRow -- psScalarTypeRow ~ source
  => ValidateExclusive
     ( description :: Optional String
     , fields :: RequiredField
     )
     objectRow
  => Row.FetchField "fields" objectRow (Row.FetchSuccess (Record objectFieldsRow) rest)
  => ValidateObjectFields psScalarTypeRow psFnFl objectFieldsRow
  => Proxy psType
  -> Record objectRow
  -> Unit -- TODO class InjectName, class InjectType, class ToGraphQLType
objectTypeTest _ _ = unit


newtype User = User
  { id :: String
  , posts :: { limit :: Int } -> Array Post
  }
derive instance genericUser :: Generic User _

newtype Post = Post
  { id :: String
  }
derive instance genericPost :: Generic Post _

objectTypeTestExample = objectTypeTest
                        (Proxy :: Proxy User)
                        { fields:
                          { id:
                              { description : ""
                              , resolve:
                                \({source: { id }}) -> pure id
                              }
                          , posts:
                              { description : ""
                              , resolve:
                                \({source: { id }, args: { limit }}) -> pure []
                              }
                          }
                        , description: "type declaration for User"
                        }


-- NOTE use isomorphism to constrain open record to closed record
-- class TestParseFn fn i o | fn -> i o, i o -> fn

-- instance testParseFn :: TestParseFn (i -> o) i o

-- test
--   :: forall psType args o
--    . TestParseFn psType args o
--   => Proxy psType
--   -> (args -> o)
--   -> Unit
-- test _ _ = unit

-- testExample = test (Proxy :: Proxy ({ id :: String } -> String))
--                    (\({id}) -> id
--                    )
