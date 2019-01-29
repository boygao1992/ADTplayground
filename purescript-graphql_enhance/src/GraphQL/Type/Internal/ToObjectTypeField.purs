module GraphQL.Type.Internal.ToObjectTypeField where

import Prelude

import Data.Generic.Rep (class Generic, Constructor, Argument)
import Data.Maybe (Maybe)
import Data.NonEmpty (NonEmpty)
import Data.Nullable (Nullable, toNullable)
import Effect.Aff (Aff)
import GraphQL.Type.Internal (class IsList, class IsListPred, class IsScalar, class IsScalarPred, GraphQLType, toList, toScalar, objectType)
import GraphQL.Type.Internal.ToInputObject (class ToInputObjectWithPath, toInputObjectWithPath)
import Prim.Row as Row
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Data.Boolean (BProxy(..))
import Type.Data.Boolean as Bool
import Type.Data.List (LProxy(..))
import Type.Data.List as List
import Type.Data.Symbol (SProxy(..))
import Type.Data.Symbol as Symbol
import Type.Proxy (Proxy(..))
import Type.Row (RProxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Type.Utils as Type


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


-- | ToObject
class ToObject spec (resolvers :: # Type) (deps :: # Type)
  | spec -> resolvers
  , spec -> deps
  where
    -- TODO  `Proxy spec` can be removed
  toObject :: Proxy spec -> Record resolvers -> Record deps -> GraphQLType spec

instance toObjectImpl ::
  ( Generic spec (Constructor specName (Argument (Record specRow)))
  , Symbol.IsSymbol specName
  , RowList.RowToList specRow specRl
  , ToFieldList specRl specFl
  , FetchScalarFields specFl source
  , ToResolvers source specFl resolvers
  , ToDeps specFl deps
  , ToObjectRow specFl specName source resolvers deps to
  ) => ToObject spec resolvers deps
  where
    toObject _ rs ds =
      objectType
        { name: Symbol.reflectSymbol (SProxy :: SProxy specName)
        , fields:
          Builder.build
          ( toObjectRow
            (LProxy :: LProxy specFl)
            (SProxy :: SProxy specName)
            (RProxy :: RProxy source)
            rs
            ds
          )
          {}
        }

class ToObjectRow
  (specFl :: List.List) (specName :: Symbol) (source :: # Type) (resolvers :: # Type) (deps :: # Type)
  (to :: # Type)
  | specFl specName source resolvers deps -> to
  where
    toObjectRow
      :: LProxy specFl -> SProxy specName
         -> RProxy source -> Record resolvers -> Record deps
      -> Builder {} (Record to)

instance toObjectRowNil ::
  ToObjectRow List.Nil specName source resolvers deps ()
  where
    toObjectRow _ _ _ _ _ = identity
else instance toObjectRowConsScalarNoArg ::
  ( ToObjectRow restFl specName source resolvers deps restTo
  , ToObjectRowDispatch specName name argType typ fieldType target source resolvers deps fieldRow
  , Row.Cons name (Record fieldRow) restTo to
  , Row.Lacks name restTo
  , Symbol.IsSymbol name
  ) => ToObjectRow (List.Cons (Field name argType typ fieldType target) restFl) specName source resolvers deps to
  where
    toObjectRow _ _ _ rs ds
        = Builder.insert
            (SProxy :: SProxy name)
            ( toObjectRowDispatch
              (SProxy :: SProxy specName)
              (SProxy :: SProxy name)
              (AProxy :: AProxy argType)
              (Proxy :: Proxy typ)
              (FTProxy :: FTProxy fieldType)
              (Proxy :: Proxy target)
              (RProxy :: RProxy source)
              rs
              ds
            )
      <<< toObjectRow
            (LProxy :: LProxy restFl)
            (SProxy :: SProxy specName)
            (RProxy :: RProxy source)
            rs
            ds

class ToObjectRowDispatch
  (specName :: Symbol) (name :: Symbol) (argType :: ArgType) typ (fieldType :: FieldType) target
  (source :: # Type) (resolvers :: # Type) (deps :: # Type)
  (fieldRow :: # Type)
  | specName name argType typ fieldType target source resolvers deps -> fieldRow
  where
    toObjectRowDispatch
      :: SProxy specName -> SProxy name -> AProxy argType -> Proxy typ
         -> FTProxy fieldType -> Proxy target -> RProxy source
      -> Record resolvers -> Record deps
      -> Record fieldRow

instance toObjectRowDispatchScalarNoArg ::
  ( ToScalarObjectFieldNoArg source typ resolve fieldRow
  , Symbol.IsSymbol name
  , Row.Cons name (Maybe resolve) restResolvers resolvers
  ) => ToObjectRowDispatch specName name NoArg typ ScalarField target source resolvers deps fieldRow
  where
    toObjectRowDispatch _ _ _ _ _ _ _ rs ds =
      toScalarObjectFieldNoArg
        (RProxy :: RProxy source)
        (Proxy :: Proxy typ)
        (Record.get (SProxy :: SProxy name) rs)
else instance toObjectRowDispatchScalarWithArgs ::
  ( Symbol.Append specName "_" path0
  , Symbol.Append path0 name path
  , ToScalarObjectFieldWithArgs path source i typ resolve fieldRow
  , Symbol.IsSymbol name
  , Row.Cons name resolve restResolvers resolvers
  ) => ToObjectRowDispatch specName name (WithArgs (Record i)) typ ScalarField target source resolvers deps fieldRow
  where
    toObjectRowDispatch _ _ _ _ _ _ _ rs ds =
      toScalarObjectFieldWithArgs
        (SProxy :: SProxy path)
        (RProxy :: RProxy source)
        (RProxy :: RProxy i)
        (Proxy :: Proxy typ)
        (Record.get (SProxy :: SProxy name) rs)
else instance toObjectRowDispatchRelationalNoArgs ::
  ( Generic target (Constructor targetName (Argument (Record targetRow)))
  , RowList.RowToList targetRow targetRl
  , ToFieldList targetRl targetFl
  , FetchScalarFields targetFl targetScalars
  , ToRelationalObjectFieldNoArg source typ target targetScalars resolve fieldRow
  , Symbol.IsSymbol name
  , Row.Cons name resolve restResolvers resolvers
  , Symbol.IsSymbol targetName
  , Row.Cons targetName (Unit -> Nullable(GraphQLType target)) restDeps deps
  ) => ToObjectRowDispatch specName name NoArg typ RelationalField target source resolvers deps fieldRow
  where
    toObjectRowDispatch _ _ _ _ _ _ _ rs ds =
      toRelationalObjectFieldNoArg
        (RProxy :: RProxy source)
        (Proxy :: Proxy typ)
        (Proxy :: Proxy target)
        (RProxy :: RProxy targetScalars)
        (Record.get (SProxy :: SProxy name) rs)
        (Record.get (SProxy :: SProxy targetName) ds)
else instance toObjectRowDispatchRelationalWithArgs ::
  ( Generic target (Constructor targetName (Argument (Record targetRow)))
  , RowList.RowToList targetRow targetRl
  , ToFieldList targetRl targetFl
  , FetchScalarFields targetFl targetScalars
  , Symbol.Append specName "_" path0
  , Symbol.Append path0 name path
  , ToRelationalObjectFieldWithArgs path source i typ target targetScalars resolve fieldRow
  , Symbol.IsSymbol name
  , Row.Cons name resolve restResolvers resolvers
  , Symbol.IsSymbol targetName
  , Row.Cons targetName (Unit -> Nullable(GraphQLType target)) restDeps deps
  ) => ToObjectRowDispatch specName name (WithArgs (Record i)) typ RelationalField target source resolvers deps fieldRow
  where
    toObjectRowDispatch _ _ _ _ _ _ _ rs ds =
      toRelationalObjectFieldWithArgs
        (SProxy :: SProxy path)
        (RProxy :: RProxy source)
        (RProxy :: RProxy i)
        (Proxy :: Proxy typ)
        (Proxy :: Proxy target)
        (RProxy :: RProxy targetScalars)
        (Record.get (SProxy :: SProxy name) rs)
        (Record.get (SProxy :: SProxy targetName) ds)


---- | ToResolvers
class ToResolvers (source :: # Type) (fl :: List.List) (resolvers :: # Type)
  | source fl -> resolvers

instance toResolversNil ::
  ToResolvers source List.Nil ()
else instance toResolversConsScalarNoArg ::
  ( ToResolversDispatch source argType typ fieldType target resolve
  , ToResolvers source restFl restResolvers
  , Row.Cons name resolve restResolvers resolvers
  ) => ToResolvers source (List.Cons (Field name argType typ fieldType target) restFl) resolvers

class ToResolversDispatch
  (source :: # Type) (argType :: ArgType) typ (fieldType :: FieldType) target
  resolve
  | source argType typ fieldType target -> resolve

instance toResolversScalarNoArg ::
  ( ToScalarObjectFieldNoArg source typ resolve fieldRow
  ) => ToResolversDispatch source NoArg typ ScalarField target (Maybe resolve)
else instance toResolversScalarWithArgs ::
  ( ToScalarObjectFieldWithArgs "" source i typ resolve fieldRow
  ) => ToResolversDispatch source (WithArgs (Record i)) typ ScalarField target resolve
else instance toResolversRelationalNoArg ::
  ( Generic target (Constructor targetName (Argument (Record targetRow)))
  , RowList.RowToList targetRow targetRl
  , ToFieldList targetRl targetFl
  , FetchScalarFields targetFl targetScalars
  , ToRelationalObjectFieldNoArg source typ target targetScalars resolve fieldRow
  ) => ToResolversDispatch source NoArg typ RelationalField target resolve
else instance toResolversRelationalWithArgs ::
  ( Generic target (Constructor targetName (Argument (Record targetRow)))
  , RowList.RowToList targetRow targetRl
  , ToFieldList targetRl targetFl
  , FetchScalarFields targetFl targetScalars
  , ToRelationalObjectFieldWithArgs "" source i typ target targetScalars resolve fieldRow
  ) => ToResolversDispatch source (WithArgs (Record i)) typ RelationalField target resolve

---- | ToDeps
class ToDeps (fl :: List.List) (deps :: # Type) | fl -> deps

instance toDepsNub ::
  ( ToDepsImpl fl deps'
  , Row.Nub deps' deps
  ) => ToDeps fl deps

class ToDepsImpl (fl :: List.List) (deps :: # Type) | fl -> deps
instance toDepsNil ::
  ToDepsImpl List.Nil ()
else instance toDepsCons ::
  ( IsScalarPred target isScalar
  , ToDepsImplDispatch isScalar target restFl deps
  ) => ToDepsImpl (List.Cons (Field name argType typ fieldType target) restFl) deps

class ToDepsImplDispatch (isScalar :: Bool.Boolean) target (restFl :: List.List) (deps :: # Type)
  | isScalar target restFl -> deps

instance toDepsImplDispatchIsScalar ::
  ( ToDepsImpl restFl deps
  ) => ToDepsImplDispatch Bool.True target restFl deps
else instance toDepsImplDispatchIsRelational ::
  ( ToDepsImpl restFl restDeps
  , Generic target (Constructor targetName (Argument (Record targetRow)))
  , Row.Cons targetName (Unit -> Nullable (GraphQLType target)) restDeps deps
  ) => ToDepsImplDispatch Bool.False target restFl deps


-- | ToScalarObjectField
class ToScalarObjectFieldNoArg
  (source :: # Type) typ
  resolve
  (fieldRow :: # Type)
  | source typ -> resolve
  , source typ -> fieldRow
  where
    toScalarObjectFieldNoArg
      :: RProxy source -> Proxy typ
      -> Maybe resolve
      -> Record fieldRow

instance toScalarObjectFieldNoArgImpl ::
  ( ToScalarObjectFieldHandleList typ
  -- TODO experiment
  -- TODO solve Nullable and Maybe conversion
  , Type.IsEqual ({ source :: Record source } -> Aff typ) resolve
  ) => ToScalarObjectFieldNoArg
    source typ
    resolve
    -- ({ source :: Record source } -> Aff typ)
    ( "type" :: GraphQLType typ
    , resolve :: Nullable (resolve)
    )
    -- ( "type" :: GraphQLType typ
    -- , resolve :: Nullable ({ source :: Record source } -> Aff typ)
    -- )
    where
      toScalarObjectFieldNoArg _ _ resolveFn =
        { "type": toScalarObjectFieldHandleList (Proxy :: Proxy typ)
        , resolve: toNullable resolveFn
        }

class ToScalarObjectFieldWithArgs
      (path :: Symbol) (source :: # Type) (i :: # Type) typ
      resolve
      (fieldRow :: # Type)
  | path source i typ -> resolve
  , path source i typ -> fieldRow
  where
    toScalarObjectFieldWithArgs
      :: SProxy path -> RProxy source -> RProxy i -> Proxy typ
      -> resolve
      -> Record fieldRow

instance toScalarObjectFieldWithArgsImpl ::
  ( ToInputObjectWithPath path i o args
  , ToScalarObjectFieldHandleList typ
  -- TODO experiment
  -- TODO solve Nullable and Maybe conversion
  , Type.IsEqual ({ source :: Record source, args :: Record args} -> Aff typ) resolve
  ) => ToScalarObjectFieldWithArgs
  path source i typ
  resolve
  -- ({ source :: Record source, args :: Record args} -> Aff typ)
  ( "type" :: GraphQLType typ
  , args :: Record o
  , resolve :: resolve
  )
  -- ( "type" :: GraphQLType typ
  -- , args :: Record o
  -- , resolve :: { source :: Record source, args :: Record args} -> Aff typ
  -- )
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

instance toScalarObjectFieldHandleListIsListPred ::
  ( IsListPred typ isList
  , ToScalarObjectFieldHandleListDispatch isList typ
  ) => ToScalarObjectFieldHandleList typ
  where
    toScalarObjectFieldHandleList _ =
      toScalarObjectFieldHandleListDispatch
        (BProxy :: BProxy isList)
        (Proxy :: Proxy typ)

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
  (source :: # Type) typ target (targetScalars :: # Type)
  resolve
  (fieldRow :: # Type)
  | source typ target targetScalars -> resolve
  , source typ target targetScalars -> fieldRow
  where
    toRelationalObjectFieldNoArg
      :: RProxy source -> Proxy typ -> Proxy target -> RProxy targetScalars
      -> resolve
      -> (Unit -> Nullable(GraphQLType target))
      -> Record fieldRow

instance toRelationalObjectFieldNoArgImpl ::
  ( ToRelationalObjectFieldHandleDepList typ (Nullable (GraphQLType target)) gType
  -- TODO experiment
  -- TODO solve Nullable and Maybe conversion
  , ToRelationalObjectFieldHandleOutputList typ targetScalars output
  , Type.IsEqual ({ source :: Record source } -> Aff output) resolve
  ) => ToRelationalObjectFieldNoArg
    source typ target targetScalars
    resolve
    -- ({ source :: Record source } -> Aff (Record targetScalars))
    ( "type" :: gType
    , resolve :: resolve
    )
    -- ( "type" :: gType
    -- , resolve :: { source :: Record source } -> Aff (Record targetScalars)
    -- )
  where
    toRelationalObjectFieldNoArg _ _ _ _ resolveFn depFn =
      { "type": toRelationalObjectHandleDepList (Proxy :: Proxy typ) depFn
      , resolve: resolveFn
      }

class ToRelationalObjectFieldWithArgs
  (path :: Symbol) (source :: # Type) (i :: # Type) typ target (targetScalars :: # Type)
  resolve
  (fieldRow :: # Type)
  | source i typ target targetScalars -> resolve
  , source i typ target targetScalars -> fieldRow
  where
    toRelationalObjectFieldWithArgs
      :: SProxy path -> RProxy source -> RProxy i -> Proxy typ
         -> Proxy target -> RProxy targetScalars
      -> resolve
      -> (Unit -> Nullable(GraphQLType target))
      -> Record fieldRow

instance toRelationalObjectFieldWithArgsImpl ::
  ( ToInputObjectWithPath path i o args
  , ToRelationalObjectFieldHandleDepList typ (Nullable (GraphQLType target)) gType
  , ToRelationalObjectFieldHandleOutputList typ targetScalars output
  -- TODO experiment
  -- TODO solve Nullable and Maybe conversion
  , Type.IsEqual ({ source :: Record source, args :: Record args} -> Aff output) resolve
  ) => ToRelationalObjectFieldWithArgs
        path source i typ target targetScalars
        resolve
        -- ({ source :: Record source, args :: Record args} -> Aff (Record targetScalars))
        ( "type" :: gType
        , args :: Record o
        , resolve :: resolve
        -- ( "type" :: gType
        -- , args :: Record o
        -- , resolve :: { source :: Record source, args :: Record args} -> Aff (Record targetScalars)
        )
  where
    toRelationalObjectFieldWithArgs _ _ _ _ _ _ resolveFn depFn =
      { "type": toRelationalObjectHandleDepList (Proxy :: Proxy typ) depFn
      , args: toInputObjectWithPath
                (SProxy :: SProxy path)
                (RProxy :: RProxy i)
      , resolve: resolveFn
      }

---- | ToRelationalObjectFieldHandleDepList
class ToRelationalObjectFieldHandleDepList i dep o | i dep -> o where
  toRelationalObjectHandleDepList :: Proxy i -> (Unit -> dep) -> o

instance toRelationalObjectFieldHandleDepListIsListPred ::
  ( IsListPred i isList
  , ToRelationalObjectFieldHandleDepListDispatch isList i dep o
  ) => ToRelationalObjectFieldHandleDepList i dep o
  where
    toRelationalObjectHandleDepList _ depFn
      = toRelationalObjectFieldHandleDepListDispatch
          (BProxy :: BProxy isList)
          (Proxy :: Proxy i)
          depFn

class ToRelationalObjectFieldHandleDepListDispatch
  (isList :: Bool.Boolean) i dep o
  | isList i dep -> o
  where
    toRelationalObjectFieldHandleDepListDispatch
      :: BProxy isList -> Proxy i -> (Unit -> dep) -> o

instance toRelationalObjectFieldHandleDepListDispatchIsList ::
  ( ToRelationalObjectFieldHandleDepList a dep (GraphQLType restO)
  , IsList f restO
  ) => ToRelationalObjectFieldHandleDepListDispatch
    Bool.True (f a) dep (GraphQLType (f restO))
  where
    toRelationalObjectFieldHandleDepListDispatch _ _ depFn
      = toList
        ( toRelationalObjectHandleDepList
            (Proxy :: Proxy a)
            depFn
        )
else instance toRelationalObjectFieldHandleDepListDispatchBaseCase ::
  ToRelationalObjectFieldHandleDepListDispatch
    Bool.False a (Nullable dep) dep -- NOTE replace a by (Nullable dep)
  where
    toRelationalObjectFieldHandleDepListDispatch _ _ depFn
      = unsafeCoerce (depFn unit) -- HACK unsafely drop Nullable

---- | ToRelationalObjectFieldHandleOutputList
class ToRelationalObjectFieldHandleOutputList i (targetScalars :: # Type) o | i targetScalars -> o

instance toRelationalObjectFieldHandleOutputListIsListPred ::
  ( IsListPred i isList
  , ToRelationalObjectFieldHandleOutputListDispatch isList i targetScalars o
  ) => ToRelationalObjectFieldHandleOutputList i targetScalars o

class ToRelationalObjectFieldHandleOutputListDispatch
  (isList :: Bool.Boolean) i (targetScalars :: # Type) o
  | isList i targetScalars -> o

instance toRelationalObjectFieldHandleOutputListIsList ::
  ( ToRelationalObjectFieldHandleOutputList a targetScalars restO
  ) => ToRelationalObjectFieldHandleOutputListDispatch
    Bool.True (f a) targetScalars (f restO)
else instance toRelationalObjectFieldHandleOutputListNotList ::
  ToRelationalObjectFieldHandleOutputListDispatch
    Bool.False a targetScalars (Record targetScalars) -- NOTE replace a by targetScalars

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
else instance fetchScalarFieldsConsIsScalar ::
  ( FetchScalarFields restList restFields
  , Row.Cons name typ restFields fields
  ) => FetchScalarFields (List.Cons (Field name argType typ ScalarField target) restList) fields
else instance fetchScalarFieldsConsNotScalar ::
  ( FetchScalarFields restList fields
  ) => FetchScalarFields (List.Cons (Field name argType typ RelationalField target) restList) fields

