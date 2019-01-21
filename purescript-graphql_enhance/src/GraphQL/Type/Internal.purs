module GraphQL.Type.Internal where

-- TODO fields in source are all scalars
-- TODO add Context (ctx)

import Prelude

import Data.Function.Uncurried (Fn1)
import Data.Generic.Rep (class Generic, Argument, Constructor)
import Effect.Aff (Aff)
import GraphQL.Data.FieldList as FieldList
import GraphQL.Type (class IsScalarPred, class ParseRelation, ObjectType, kind Relation)
import Prim.TypeError (class Fail, Text, Quote, Above, Beside)
import Type.Data.Boolean as Bool
import Type.Proxy (Proxy(..))
import Type.Row (RProxy(..))
import Type.Row (class Cons) as Row
import Type.Row.Utils (class FetchField, FetchFailure, FetchSuccess, kind FetchResult) as Row
import Type.Row.Validation (class Validate, Optional, Repelled, Required, Success)

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
  , Row.Cons name (Record objectFieldRow) restObjectFieldsRow objectFieldsRow
  , ValidateObjectField isScalar name args rela a source objectFieldRow
  , ValidateObjectFields source restPsFnFl objectFieldsRow
  ) => ValidateObjectFields source (FieldList.Cons name args rela a restPsFnFl) objectFieldsRow

-- | ValidateObjectField
class ValidateObjectField (isScalar :: Bool.Boolean) (name :: Symbol) (args :: FieldList.Arguments) (rela :: Relation) a  (source :: # Type) (objectFieldRow :: # Type)

instance validateObjectFieldNoArgs ::
  ( ParseRelation typ rela a -- typ <- rela a
  , Validate ( description :: Optional String ) objectFieldRow Success
  , Row.FetchField "resolve" objectFieldRow resolveFn
  , ValidateObjectFieldNoArgs resolveFn source typ
  ) => ValidateObjectField Bool.True name FieldList.NoArgs rela a source objectFieldRow
else instance validateObjectFieldWithArgs ::
  ( ParseRelation typ rela a -- typ <- rela a
  , Validate ( description :: Optional String ) objectFieldRow Success
  , Row.FetchField "resolve" objectFieldRow resolveFn
  , ValidateObjectFieldWithArgs name resolveFn args source typ
  ) => ValidateObjectField Bool.True name (FieldList.Args args) rela a source objectFieldRow
else instance validateObjectFieldNotScalarNoArgs ::
  ( GenericToScalarTypeRow a aScalarRow
  , ParseRelation typ rela (Record aScalarRow) -- typ <- rela (Record aScalarRow)
  , Validate ( description :: Optional String ) objectFieldRow Success
  , Row.FetchField "resolve" objectFieldRow resolveFn
  , ValidateObjectFieldNoArgs resolveFn source typ
  ) => ValidateObjectField Bool.False name FieldList.NoArgs rela a source objectFieldRow
else instance validateObjectFieldNotScalarWithArgs ::
  ( GenericToScalarTypeRow a aScalarRow
  , ParseRelation typ rela (Record aScalarRow) -- typ <- rela (Record aScalarRow)
  , Validate ( description :: Optional String ) objectFieldRow Success
  , Row.FetchField "resolve" objectFieldRow resolveFn
  , ValidateObjectFieldWithArgs name resolveFn args source typ
  ) => ValidateObjectField Bool.False name (FieldList.Args args) rela a source objectFieldRow

class ValidateObjectFieldNoArgs (resolveFn :: Row.FetchResult) (source :: # Type) typ

instance validateObjectFieldNoArgsNoResolve ::
  ValidateObjectFieldNoArgs Row.FetchFailure source typ
else instance validateObjectFieldNoArgsWithResolve ::
  ( ValidateObjectFieldNoArgsSuccess resolve source typ
  ) => ValidateObjectFieldNoArgs (Row.FetchSuccess (resolve) rest) source typ

class ValidateObjectFieldNoArgsSuccess (resolve :: Type) (source :: # Type) typ | resolve -> source typ, source typ -> resolve

instance validateObjectFieldNoArgsSuccess ::
  ( Row.Cons "source" (Record source) () i
  ) => ValidateObjectFieldNoArgsSuccess (Record i -> Aff typ) source typ

class ValidateObjectFieldWithArgs (name :: Symbol) (resolveFn :: Row.FetchResult) (args :: # Type) (source :: # Type) typ

instance validateObjectFieldWithArgsNoResolve ::
  Fail
  ( Above
    (Beside (Text "Missing resolve function for field `") (Beside (Text name) (Text "`:")))
    (Quote ({source :: Record source, args :: Record args} -> Aff typ))
  )
  => ValidateObjectFieldWithArgs name Row.FetchFailure args source typ
else instance validateObjectFieldWithArgsWithResolve ::
  ( ValidateObjectFieldWithArgsSuccess resolve args source typ
  ) => ValidateObjectFieldWithArgs name (Row.FetchSuccess resolve rest) args source typ

class ValidateObjectFieldWithArgsSuccess (resolve :: Type) (args :: # Type) (source :: # Type) typ | resolve -> args source typ, args source typ -> resolve

instance validateObjectFieldWithArgsSuccess ::
  -- Fail
  -- ( Quote typ)
  -- => ValidateObjectFieldWithArgsSuccess (Record i -> Aff tye) args source typ
  ( Row.Cons "source" (Record source) i0 i
  , Row.Cons "args" (Record args) i1 i
  ) => ValidateObjectFieldWithArgsSuccess (Record i -> Aff typ) args source typ



-- | objectType

objectType
  :: forall objectRow deps psType psTypeName psFnRow psFnFl psScalarTypeRow
     objectFieldsRow
     rest
   . Generic psType (Constructor psTypeName (Argument (Record psFnRow)))
  => FieldList.FromRow psFnRow psFnFl
  => FieldList.ToScalarTypeRow psFnFl psScalarTypeRow -- psScalarTypeRow ~ source
  => Validate (description :: Optional String) objectRow Success
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
  => Validate (description :: Optional String) objectRow Success
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
