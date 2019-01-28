module GraphQL.Type.Internal.ToObjectTypeField where

import GraphQL.Type.Internal (class IsList, class IsListPred, class IsScalar, class IsScalarPred, GraphQLType, toList, toScalar)
import GraphQL.Type.Internal.ToInputObject (class ToInputObjectWithPath, toInputObjectWithPath)
import Prelude

import Data.Maybe (Maybe)
import Data.NonEmpty (NonEmpty)
import Data.Nullable (Nullable, toNullable)
import Effect.Aff (Aff)
import Prim.Row as Row
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Type.Data.Boolean (BProxy(..))
import Type.Data.Boolean as Bool
import Type.Data.List as List
import Type.Data.Symbol (SProxy(..))
import Type.Proxy (Proxy(..))
import Type.Row (RProxy(..))
import Unsafe.Coerce (unsafeCoerce)


-- NOTE refactoring of ValidateMissingFields with better organized constructs
-- currently hard to implement value-level transformations

{-
newtype User = User
  { id :: String
  , posts :: { date :: String } -> Array Post
  , comments :: { limit :: Int} -> Array Comment
  }


{ "Post" :: Unit -> Nullable(GraphQLType(Post))
, "Comment" :: Unit -> Nullable(GraphQLType(Comment))
}
=>
G.object
{ name: "User"
, fields:
  { id: { type: G.string }
  , posts:
    { type: G.list(Post())
    , args:
      { date: { type: G.string }
      }
    , resolve:
    }
  , comments:
    { type: G.list(Comment())
    , args:
      { limit: { type: G.int }
      }
    , resolve:
    }
  }
}

newtype Post = Post
  { id :: String
  , author :: User
  , comments :: { limit :: Int } -> Array Comment
  }

newtype Comment = Comment
  { id :: String
  , author :: User
  , post :: Post
  }

-}

{-
rules for resolvers
- if a field has args definition in its fieldSpec
  - then its resolver is Required
  - else
    - if the field is Scalar
      , or the field is Relational and all its fields have a resolver Required
        - NOTE cyclic dependency alert
        - e.g. User { post :: Post }, Post { author :: User }
          to see if resolver for field `post` in User is Required
          we need to know if resolver for field `author` in Post is Required
          which then come back to the question itself
      - then its resolver is Optional
      - else its resolver is Required

updated rules for resolvers with cyclic dependency avoided
- if a field has args definition in its fieldSpec
  - then its resolver is Required
  - else
    - case1: the field is Scalar
      - then its resolver is Optional
    - case2: the field is Relational
      - then its resolver is Required
    - otherwise: TypeError

NOTE extra rules to forbid trivial resolvers to be Required
- an Entity must have at least one Scalar field
  - which doesn't have a resolver itself
  - but is resolved from its parent
e.g. User { id :: String, post :: Post }, Post { id :: { id :: String } -> String }
  the resolver for field `post` in User is trivial:
    postResolver :: { source:: {} } -> Aff {}
  which takes in an empty Record and produces an empty Record

newtype User = User
  { id :: String
  , posts :: { date :: String } -> Array Post
  , comments :: { limit :: Int } -> Array Comment
  }
newtype Post = Post
  { id :: String
  , author :: User
  , comments :: { limit :: Int } -> Array Comment
  }
newtype Comment = Comment
  { id :: String
  , author :: User
  , post :: Post
  }

variables
- Generic entity (Constructor entityName (Argument spec))
- spec :: # Type
  - source
  - argsRow
  - outputRow
  - fieldTypeRow
  - resolversSchema <- source, argsRow, outputRow
  - deps <- fieldTypeRow
- resolvers :: # Type
- deps :: # Type
- o = GraphQLType spec

source = { id :: String }

argsRow =
  { id :: NoArg
  , posts :: WithArgs { date :: String }
  , comments :: WithArgs { limit :: Int }
  }
outputRow =
  { id :: String
  , posts :: Array { id :: String }
  , comments :: Array { id :: String }
  }
fieldTypeRow =
  { id :: ScalarField
  , posts :: RelationalField Post
  , comments :: RelationalField Comment
  }
argsRow + outputRow =
  { id :: ObjectField NoArg String
  , posts :: ObjectField (WithArgs { date :: String }) (Array { id :: String })
  , comments :: ObjectField (WithArgs { id :: String }) (Array { id :: String })
  }

resolversSchema =
  { id :: Optional
      { source :: source
      }
      -> Aff String
  , posts :: Required
      { source :: source
      , args :: { date :: String }
      }
      -> Aff (Array postScalars)
  , comments :: Required
      { source :: source
      , args :: { limit :: Int }
      }
      -> Aff (Array commentScalars)
  }

resolvers =
  { id :: Maybe
      ( { source :: source
        }
        -> Aff String
      )
  , posts ::
      { source :: source
      , args :: { date :: String }
      }
      -> Aff (Array postScalars)
  , comments ::
      { source :: source
      , args :: { limit :: Int }
      }
      -> Aff (Array commentScalars)
  }

deps =
  { "Post" :: Unit -> Nullable(GraphQLType Post)
  , "Comment" :: Unit -> Nullable(GraphQLType Comment)
  }


GraphQLType User =
G.object
{ name: "User"
, fields: \_ ->
  { id:
    { type: G.string }
  , posts:
    { type: G.list( deps."Post" unit )
    , args:
      { date: { type: G.string }
      }
    , resolve: resolvers.posts
    }
  , comments:
    { type: G.list( deps."Comment" unit )
    , args:
      { limit: { type: G.int }
      }
    , resolve: resolvers.comments
    }
  }
}

-}

{- ToObject

input
- entity :: Type

entity = User { id :: String, posts :: { date :: String } -> Array Post }
- specRow <- entity
  - Generic entity (Constructor name (Argument (Record specRow)))
- specRl <- specRow
  - RowToList
  - Row.Cons fieldName fieldSpec restSpecRl
- resolve, dep, fieldRow <- fieldSpec

-}

{- ToObjectField

input
- path :: Symbol
- source :: Type
- fieldSpec :: Type

output
- resolve :: Type
- target :: Type
- fieldRow :: # Type

fieldSpec = { date :: String } -> Array Post
- i = { data :: String }
  - args <- UnwrapNewtype i
- o = Array Post
  - target <- o
    = Post
  - Generic target (Constructor targetName (Argument (Record targetRow)))
  - targetScalars <- targetRow
    = { id :: String }
  - dep <- target
    = Unit -> Nullable(GraphQLType Post)
- resolve <- args, postScalars
  = { source :: source, args :: args } -> Aff postScalars

toObjectField
  :: ( { source :: source, args :: args } -> Aff targetScalars )
  -> ( Unit -> Nullable(GraphQLType target) )
  -> Record
      ( type :: Nullable(GraphQLType Post)
      , args :: GraphQLType i <= ToInputObjectWithPath path
      , resolve :: resolve
      )

-}

{-
--- type-level ---
1. first pass, parse type info for each field and gather them into a List
  - fieldName
  - args <= toInputObjectWithPath (path i -> o args)
  - fieldType
  - target
2. get source = scalarFields of input type <- List
3. get resolvers and deps both of which are type info of toObject
--- value-level ---
4. dispatch resolvers Record and deps Record to each field
-}



-- | ToScalarObjectField
class ToScalarObjectFieldNoArg
  source typ
  resolve
  (fieldRow :: # Type)
  | source typ -> resolve
  , resolve -> fieldRow
  where
    toScalarObjectFieldNoArg
      :: Proxy source -> Proxy typ
      -> Maybe resolve
      -> Record fieldRow

instance toScalarObjectFieldNoArgImpl ::
  ( ToScalarObjectFieldHandleList typ
  ) => ToScalarObjectFieldNoArg
    source typ
    ({ source :: source } -> Aff typ)
    ( "type" :: GraphQLType typ
    , resolve :: Nullable ({ source :: source } -> Aff typ)
    )
    where
      toScalarObjectFieldNoArg _ _ resolveFn =
        { "type": toScalarObjectFieldHandleList (Proxy :: Proxy typ)
        , resolve: toNullable resolveFn
        }

class ToScalarObjectFieldWithArgs
      (path :: Symbol) source (i :: # Type) typ
      resolve
      (fieldRow :: # Type)
  | path source i typ -> resolve
  , resolve -> fieldRow
  where
    toScalarObjectFieldWithArgs
      :: SProxy path -> Proxy source -> RProxy i -> Proxy typ
      -> resolve
      -> Record fieldRow

instance toScalarObjectFieldWithArgsImpl ::
  ( ToInputObjectWithPath path i o args
    , ToScalarObjectFieldHandleList typ
  ) => ToScalarObjectFieldWithArgs
  path source i typ
  ({ source :: source, args :: Record args} -> Aff typ)
  ( "type" :: GraphQLType typ
  , args :: Record o
  , resolve :: { source :: source, args :: Record args} -> Aff typ
  )
  where
    toScalarObjectFieldWithArgs _ _ _ _ resolveFn =
      { "type": toScalarObjectFieldHandleList (Proxy :: Proxy typ)
      , args: toInputObjectWithPath
        (SProxy :: SProxy path)
        (RProxy :: RProxy i)
      , resolve: resolveFn
      }

---- | ToScalarObjectFieldHandleList
class ToScalarObjectFieldHandleList typ
  where
    toScalarObjectFieldHandleList :: Proxy typ -> GraphQLType typ

class ToScalarObjectFieldHandleListDispatch
  (isList :: Bool.Boolean) typ
  where
    toScalarObjectFieldHandleListDispatch :: BProxy isList -> Proxy typ -> GraphQLType typ

instance toScalarObjectFieldHandleListDispatchIsList ::
  ( ToScalarObjectFieldHandleList a
  , IsList f a
  ) => ToScalarObjectFieldHandleListDispatch Bool.True (f a)
  where
    toScalarObjectFieldHandleListDispatch _ _
      = toList
        ( toScalarObjectFieldHandleList
            (Proxy :: Proxy a)
        )
else instance toScalarObjectFieldHandleListDispatchNotList ::
  ( IsScalar a
  ) => ToScalarObjectFieldHandleListDispatch Bool.False a
  where
    toScalarObjectFieldHandleListDispatch _ _ = toScalar

-- | ToRelationalObjectField
class ToRelationalObjectFieldNoArg
  source typ target targetScalars
  resolve
  (fieldRow :: # Type)
  | source typ target targetScalars -> resolve
  , target resolve -> fieldRow
  where
    toRelationalObjectFieldNoArg
      :: Proxy source -> Proxy typ -> Proxy target -> Proxy targetScalars
      -> resolve
      -> (Unit -> Nullable(GraphQLType target))
      -> Record fieldRow

instance toRelationalObjectFieldNoArgImpl ::
  ( ToRelationalObjectFieldHandleList typ (Nullable (GraphQLType target)) gType
  ) => ToRelationalObjectFieldNoArg
    source typ target targetScalars
    ({ source :: source } -> Aff targetScalars)
    ( "type" :: gType
    , resolve :: { source :: source } -> Aff targetScalars
    )
  where
    toRelationalObjectFieldNoArg _ _ _ _ resolveFn depFn =
      { "type": toRelationalObjectHandleList (Proxy :: Proxy typ) depFn
      , resolve: resolveFn
      }

class ToRelationalObjectFieldWithArgs
  (path :: Symbol) source (i :: # Type) typ target targetScalars
  resolve
  (fieldRow :: # Type)
  | source i typ target targetScalars -> resolve
  , path i target resolve -> fieldRow
  where
    toRelationalObjectFieldWithArgs
      :: SProxy path -> Proxy source -> RProxy i -> Proxy typ
         -> Proxy target -> Proxy targetScalars
      -> resolve
      -> (Unit -> Nullable(GraphQLType target))
      -> Record fieldRow

instance toRelationalObjectFieldWithArgsImpl ::
  ( ToInputObjectWithPath path i o args
  , ToRelationalObjectFieldHandleList typ (Nullable (GraphQLType target)) gType
  ) => ToRelationalObjectFieldWithArgs
        path source i typ target targetScalars
        ({ source :: source, args :: Record args} -> Aff targetScalars)
        ( "type" :: gType
        , args :: Record o
        , resolve :: { source :: source, args :: Record args} -> Aff targetScalars
        )
  where
    toRelationalObjectFieldWithArgs _ _ _ _ _ _ resolveFn depFn =
      { "type": toRelationalObjectHandleList (Proxy :: Proxy typ) depFn
      , args: toInputObjectWithPath
                (SProxy :: SProxy path)
                (RProxy :: RProxy i)
      , resolve: resolveFn
      }

---- | ToRelationalObjectFieldHandleList
class ToRelationalObjectFieldHandleList i dep o | i dep -> o where
  toRelationalObjectHandleList :: Proxy i -> (Unit -> dep) -> o

instance toRelationalObjectFieldHandleListIsListPred ::
  ( IsListPred i isList
  , ToRelationalObjectFieldHandleListDispatch isList i dep o
  ) => ToRelationalObjectFieldHandleList i dep o
  where
    toRelationalObjectHandleList _ depFn
      = toRelationalObjectFieldHandleListDispatch
          (BProxy :: BProxy isList)
          (Proxy :: Proxy i)
          depFn

class ToRelationalObjectFieldHandleListDispatch
  (isList :: Bool.Boolean) i dep o
  | isList i dep -> o
  where
    toRelationalObjectFieldHandleListDispatch
      :: BProxy isList -> Proxy i -> (Unit -> dep) -> o

instance toRelationalObjectFieldHandleListDispatchIsList ::
  ( ToRelationalObjectFieldHandleList a dep (GraphQLType restO)
  , IsList f restO
  ) => ToRelationalObjectFieldHandleListDispatch
    Bool.True (f a) dep (GraphQLType (f restO))
  where
    toRelationalObjectFieldHandleListDispatch _ _ depFn
      = toList
        ( toRelationalObjectHandleList
            (Proxy :: Proxy a)
            depFn
        )
else instance toRelationalObjectFieldHandleListDispatchBaseCase ::
  ToRelationalObjectFieldHandleListDispatch
    Bool.False a (Nullable dep) dep
  where
    toRelationalObjectFieldHandleListDispatch _ _ depFn
      = unsafeCoerce (depFn unit) -- HACK unsafely drop Nullable

-- | FieldType = ScalarField | RelationalField
foreign import kind FieldType
foreign import data ScalarField :: FieldType
foreign import data RelationalField :: FieldType
data FTProxy (fieldType :: FieldType) = FTProxy

-- | Args = NoArgs | WithArgs a
foreign import kind ArgType
foreign import data NoArg :: ArgType
foreign import data WithArgs :: Type -> ArgType
data AProxy (argType :: ArgType) = AProxy

-- | Field
data Field (name :: Symbol) (argType :: ArgType) typ (fieldType :: FieldType) target

-- | ParseFieldSpec
class ParseFieldSpec
  (spec :: Type)
  (argType :: ArgType) typ
  (fieldType :: FieldType) target
  | spec -> argType typ
  , typ -> fieldType target

instance parseFieldSpecWithArgs ::
  ( ParseList o fieldType target
  ) => ParseFieldSpec ((Record i) -> o) (WithArgs (Record i)) o fieldType target
else instance parseFieldSpecNoArg ::
  ( ParseList o fieldType target
  ) => ParseFieldSpec o NoArg o fieldType target

---- | ParseList
class ParseList typ (fieldType :: FieldType) target | typ -> fieldType target

instance parseListRelationalArray ::
  ( ParseList a fieldType target
  ) => ParseList (Array a) fieldType target
else instance parseListRelationalNonEmptyArray ::
  ( ParseList a fieldType target
  ) => ParseList (NonEmpty Array a) fieldType target
else instance parseListRelationalTarget ::
  ( ToFieldType target fieldType
  ) => ParseList target fieldType target

------ | ToFieldType
class ToFieldType a (fieldType :: FieldType) | a -> fieldType

instance toFieldTypeImpl ::
  ( IsScalarPred a isScalar
  , Bool.If isScalar
      (FTProxy ScalarField)
      (FTProxy RelationalField)
      (FTProxy fieldType)
  ) => ToFieldType a fieldType

-- | ToFieldList
class ToFieldList (specRl :: RowList) (fieldList :: List.List)
  | specRl -> fieldList

instance toFieldListNil ::
  ToFieldList RowList.Nil List.Nil
else instance toFieldListCons ::
  ( ToFieldList restSpecRl restFieldList
  , ParseFieldSpec fieldSpec args typ fieldType target
  ) => ToFieldList (RowList.Cons fieldName fieldSpec restSpecRl) (List.Cons (Field fieldName args typ fieldType target) restFieldList)

-- | FetchScalarFields
class FetchScalarFields (fieldList :: List.List) (scalarFields :: # Type)
  | fieldList -> scalarFields

instance fetchScalarFieldsNil ::
  FetchScalarFields List.Nil ()
else instance fetchScalarFieldsCons ::
  ( FetchScalarFields restList restFields
  , Row.Cons name typ restFields fields
  ) => FetchScalarFields (List.Cons (Field name argType typ ScalarField target) restList) fields
