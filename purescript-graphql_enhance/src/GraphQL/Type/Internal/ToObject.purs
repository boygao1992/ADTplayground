module GraphQL.Type.Internal.ToObject where

import Prelude (identity, (<<<), Unit, map, (<$>), unit, ($))

import Control.Promise (Promise, fromAff)
import Data.Function.Uncurried (Fn3, mkFn3)
import Data.Generic.Rep (class Generic, Constructor, Argument)
import Data.Maybe (Maybe)
import Data.NonEmpty (NonEmpty)
import Data.Nullable (Nullable, toNullable)
import Effect.Aff (Aff)
import Effect.Unsafe (unsafePerformEffect)
import GraphQL.Type.Data.Field (AProxy(..), FTProxy(..), Field, NoArg, RelationalField, ScalarField, WithArgs, kind ArgType, kind FieldType)
import GraphQL.Type.Internal (class IsList, class IsListPred, class IsScalar, class IsScalarPred, Id, GraphQLType, toList, toScalar, objectType, nonNull)
import GraphQL.Type.Internal.NullableAndMaybe (class NullableAndMaybe, fromMaybeToNullable)
import GraphQL.Type.Internal.NullableAndMaybeRec (class NullableAndMaybeRec, fromNullableToMaybeRec)
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
  toObject :: Proxy spec -> Record resolvers -> Record deps
              -> GraphQLType (Maybe spec)

instance toObjectImpl ::
  ( Generic spec (Constructor specName (Argument (Record specRow)))
  , Symbol.IsSymbol specName
  , RowList.RowToList specRow specRl
  , ToFieldList specRl specFl
  , FetchScalarFields specFl sourceRow
  , ToResolvers (Record sourceRow) specFl resolvers
  , ToDeps specFl deps
  , ToObjectRow specFl specName (Record sourceRow) resolvers deps to
  ) => ToObject spec resolvers deps
  where
    toObject _ rs ds =
      objectType
        { name: Symbol.reflectSymbol (SProxy :: SProxy specName)
        , fields: \_ -> -- NOTE lazy
            Builder.build
            ( toObjectRow
              (LProxy :: LProxy specFl)
              (SProxy :: SProxy specName)
              (Proxy :: Proxy (Record sourceRow))
              rs
              ds
            )
            {}
        }

class ToObjectRow
  (specFl :: List.List) (specName :: Symbol) (source :: Type) (resolvers :: # Type) (deps :: # Type)
  (to :: # Type)
  | specFl specName source resolvers deps -> to
  where
    toObjectRow
      :: LProxy specFl -> SProxy specName
         -> Proxy source -> Record resolvers -> Record deps
      -> Builder {} (Record to)

instance toObjectRowNil ::
  ToObjectRow List.Nil specName source resolvers deps ()
  where
    toObjectRow _ _ _ _ _ = identity

instance toObjectRowCons ::
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
              (Proxy :: Proxy source)
              rs
              ds
            )
      <<< toObjectRow
            (LProxy :: LProxy restFl)
            (SProxy :: SProxy specName)
            (Proxy :: Proxy source)
            rs
            ds

class ToObjectRowDispatch
  (specName :: Symbol) (name :: Symbol) (argType :: ArgType) typ (fieldType :: FieldType) target
  (source :: Type) (resolvers :: # Type) (deps :: # Type)
  (fieldRow :: # Type)
  | specName name argType typ fieldType target source resolvers deps -> fieldRow
  where
    toObjectRowDispatch
      :: SProxy specName -> SProxy name -> AProxy argType -> Proxy typ
         -> FTProxy fieldType -> Proxy target -> Proxy source
      -> Record resolvers -> Record deps
      -> Record fieldRow

instance toObjectRowDispatchScalarNoArg ::
  ( ToScalarObjectFieldNoArg source typ resolve fieldRow
  , Symbol.IsSymbol name
  , Row.Cons name (Maybe resolve) restResolvers resolvers
  ) => ToObjectRowDispatch specName name NoArg typ ScalarField target source resolvers deps fieldRow
  where
    toObjectRowDispatch _ _ _ _ _ _ _ rs _ =
      toScalarObjectFieldNoArg
        (Proxy :: Proxy source)
        (Proxy :: Proxy typ)
        (Record.get (SProxy :: SProxy name) rs)

instance toObjectRowDispatchScalarWithArgs ::
  ( Symbol.Append specName "_" path0
  , Symbol.Append path0 name path
  , ToScalarObjectFieldWithArgs path source i typ resolve fieldRow
  , Symbol.IsSymbol name
  , Row.Cons name resolve restResolvers resolvers
  ) => ToObjectRowDispatch specName name (WithArgs (Record i)) typ ScalarField target source resolvers deps fieldRow
  where
    toObjectRowDispatch _ _ _ _ _ _ _ rs _ =
      toScalarObjectFieldWithArgs
        (SProxy :: SProxy path)
        (Proxy :: Proxy source)
        (RProxy :: RProxy i)
        (Proxy :: Proxy typ)
        (Record.get (SProxy :: SProxy name) rs)

instance toObjectRowDispatchRelationalNoArgs ::
  ( Generic target (Constructor targetName (Argument (Record targetRow)))
  , RowList.RowToList targetRow targetRl
  , ToFieldList targetRl targetFl
  , FetchScalarFields targetFl targetScalars
  , ToRelationalObjectFieldNoArg source typ target targetScalars resolve fieldRow
  , Symbol.IsSymbol name
  , Row.Cons name resolve restResolvers resolvers
  , Symbol.IsSymbol targetName
  , Row.Cons targetName (Unit -> Nullable(GraphQLType (Maybe target))) restDeps deps
  ) => ToObjectRowDispatch specName name NoArg typ RelationalField target source resolvers deps fieldRow
  where
    toObjectRowDispatch _ _ _ _ _ _ _ rs ds =
      toRelationalObjectFieldNoArg
        (Proxy :: Proxy source)
        (Proxy :: Proxy typ)
        (Proxy :: Proxy target)
        (RProxy :: RProxy targetScalars)
        (Record.get (SProxy :: SProxy name) rs)
        (Record.get (SProxy :: SProxy targetName) ds)

instance toObjectRowDispatchRelationalWithArgs ::
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
  , Row.Cons targetName (Unit -> Nullable(GraphQLType (Maybe target))) restDeps deps
  ) => ToObjectRowDispatch specName name (WithArgs (Record i)) typ RelationalField target source resolvers deps fieldRow
  where
    toObjectRowDispatch _ _ _ _ _ _ _ rs ds =
      toRelationalObjectFieldWithArgs
        (SProxy :: SProxy path)
        (Proxy :: Proxy source)
        (RProxy :: RProxy i)
        (Proxy :: Proxy typ)
        (Proxy :: Proxy target)
        (RProxy :: RProxy targetScalars)
        (Record.get (SProxy :: SProxy name) rs)
        (Record.get (SProxy :: SProxy targetName) ds)


---- | ToResolvers
class ToResolvers (source :: Type) (fl :: List.List) (resolvers :: # Type)
  | source fl -> resolvers

instance toResolversNil ::
  ToResolvers source List.Nil ()

instance toResolversCons ::
  ( ToResolversDispatch source argType typ fieldType target resolve
  , ToResolvers source restFl restResolvers
  , Row.Cons name resolve restResolvers resolvers
  ) => ToResolvers source (List.Cons (Field name argType typ fieldType target) restFl) resolvers

class ToResolversDispatch
  (source :: Type) (argType :: ArgType) typ (fieldType :: FieldType) target
  resolve
  | source argType typ fieldType target -> resolve

instance toResolversScalarNoArg ::
  ( ToScalarObjectFieldNoArg source typ resolve fieldRow
  ) => ToResolversDispatch source NoArg typ ScalarField target (Maybe resolve)

instance toResolversScalarWithArgs ::
  ( ToScalarObjectFieldWithArgs "" source i typ resolve fieldRow
  ) => ToResolversDispatch source (WithArgs (Record i)) typ ScalarField target resolve

instance toResolversRelationalNoArg ::
  ( Generic target (Constructor targetName (Argument (Record targetRow)))
  , RowList.RowToList targetRow targetRl
  , ToFieldList targetRl targetFl
  , FetchScalarFields targetFl targetScalars
  , ToRelationalObjectFieldNoArg source typ target targetScalars resolve fieldRow
  ) => ToResolversDispatch source NoArg typ RelationalField target resolve

instance toResolversRelationalWithArgs ::
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
  , Row.Cons targetName (Unit -> Nullable (GraphQLType (Maybe target))) restDeps deps
  ) => ToDepsImplDispatch Bool.False target restFl deps


-- | ToScalarObjectField
class ToScalarObjectFieldNoArg
  (source :: Type) typ
  resolve
  (fieldRow :: # Type)
  | source typ -> resolve
  , source typ -> fieldRow
  where
    toScalarObjectFieldNoArg
      :: Proxy source -> Proxy typ
      -> Maybe resolve
      -> Record fieldRow

instance toScalarObjectFieldNoArgImpl ::
  ( ToScalarObjectFieldHandleList mTyp
  -- TODO solve Id and String conversion
  -- NOTE solve Nullable and Maybe conversion
  , ToScalarObjectFieldHandleOutputList mTyp mTyp'
  , NullableAndMaybe nTyp mTyp'
  ) => ToScalarObjectFieldNoArg
    source mTyp
    -- resolve
    ({ source :: source } -> Aff mTyp')
    -- ( "type" :: GraphQLType typ
    -- , resolve :: Nullable (resolve)
    -- )
    ( "type" :: GraphQLType mTyp
    -- NOTE \source args context -> { source, args, context }
    , resolve :: Nullable
                 ( Fn3
                    (source)
                    noArg
                    noContext -- TODO add Context
                    (Promise nTyp)
                 )
                 -- ( { source :: source } -> Effect (Promise nTyp))
    )
    where
      toScalarObjectFieldNoArg _ _ resolveFn =
        { "type": toScalarObjectFieldHandleList (Proxy :: Proxy mTyp)
        , resolve: toNullable
                     (trans <$> resolveFn)
        }
        where
          trans f = mkFn3 \source _ _ ->
            -- NOTE Aff to Promise
            unsafePerformEffect <<< fromAff <<< map fromMaybeToNullable <<< f
            $ { source }

class ToScalarObjectFieldWithArgs
      (path :: Symbol) (source :: Type) (i :: # Type) typ
      resolve
      (fieldRow :: # Type)
  | path source i typ -> resolve
  , path source i typ -> fieldRow
  where
    toScalarObjectFieldWithArgs
      :: SProxy path -> Proxy source -> RProxy i -> Proxy typ
      -> resolve
      -> Record fieldRow

instance toScalarObjectFieldWithArgsImpl ::
  ( ToInputObjectWithPath path i o mArgs
  , ToScalarObjectFieldHandleList mTyp
  -- TODO solve Id and String conversion
  -- NOTE solve Nullable and Maybe conversion
  , NullableAndMaybeRec (Record nArgs) (Record mArgs)
  , ToScalarObjectFieldHandleOutputList mTyp mTyp'
  , NullableAndMaybe nTyp mTyp'
  ) => ToScalarObjectFieldWithArgs
  path source i mTyp
  ({ source :: source, args :: Record mArgs} -> Aff mTyp')
  ( "type" :: GraphQLType mTyp
  , args :: Record o
  -- NOTE \source args context -> { source, args, context }
  , resolve ::
       Fn3
         (source)
         (Record nArgs)
         noContext -- TODO addContext
         (Promise nTyp)
      -- { source :: source, args :: Record nArgs} -> Effect (Promise nTyp)
  )
  where
    toScalarObjectFieldWithArgs _ _ _ _ resolveFn =
      { "type": toScalarObjectFieldHandleList (Proxy :: Proxy mTyp)
      , args: toInputObjectWithPath
        (SProxy :: SProxy path)
        (RProxy :: RProxy i)
      , resolve:
          mkFn3 \source args _ ->
            -- NOTE Aff to Promise
                unsafePerformEffect
            <<< fromAff
            <<< map fromMaybeToNullable
            <<< resolveFn
            $ { source
              , args: fromNullableToMaybeRec args
              }
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

-- | ToScalarObjectFieldHandleOutputList
class ToScalarObjectFieldHandleOutputList i o | i -> o

instance toScalarObjectFieldHandleOutputListIsListPred ::
  ( IsListPred i isList
  , ToScalarObjectFieldHandleOutputListDispatch isList i o
  ) => ToScalarObjectFieldHandleOutputList i o

class ToScalarObjectFieldHandleOutputListDispatch
  (isList :: Bool.Boolean) i o
  | isList i -> o

instance toScalarObjectFieldHandleOutputListIsList ::
  ( ToScalarObjectFieldHandleOutputList a restO
  ) => ToScalarObjectFieldHandleOutputListDispatch Bool.True (f a) (f restO)

instance toScalarObjectFieldHandleOutputListNotListId ::
  ToScalarObjectFieldHandleOutputListDispatch Bool.False Id String
else instance toScalarObjectFieldHandleOutputListNotListMaybeId ::
  ToScalarObjectFieldHandleOutputListDispatch Bool.False (Maybe Id) (Maybe String)
else instance toScalarObjectFieldHandleOutputListNotList ::
  ToScalarObjectFieldHandleOutputListDispatch Bool.False a a

-- instance toRelationalObjectFieldHandleOutputListIsList ::
--   ( ToRelationalObjectFieldHandleOutputList a targetScalars restO
--   ) => ToRelationalObjectFieldHandleOutputListDispatch
--     Bool.True (f a) targetScalars (f restO)
-- else instance toRelationalObjectFieldHandleOutputListNotList ::
--   ToRelationalObjectFieldHandleOutputListDispatch
--     Bool.False a targetScalars (Record targetScalars) -- NOTE replace a by targetScalars

-- | ToRelationalObjectField
class ToRelationalObjectFieldNoArg
  (source :: Type) typ target (targetScalars :: # Type)
  resolve
  (fieldRow :: # Type)
  | source typ target targetScalars -> resolve
  , source typ target targetScalars -> fieldRow
  where
    toRelationalObjectFieldNoArg
      :: Proxy source -> Proxy typ -> Proxy target -> RProxy targetScalars
      -> resolve
      -> (Unit -> Nullable(GraphQLType (Maybe target)))
      -> Record fieldRow

instance toRelationalObjectFieldNoArgImpl ::
  ( ToRelationalObjectFieldHandleDepList typ target gType
  -- TODO solve Id and String conversion
  -- NOTE solve Nullable and Maybe conversion
  , NullableAndMaybe nOutput mOutput
  , ToRelationalObjectFieldHandleOutputList typ mTargetScalars mOutput
  ) => ToRelationalObjectFieldNoArg
    source typ target mTargetScalars
    ( { source :: source }
      -> Aff mOutput
    )
    ( "type" :: GraphQLType gType
    , resolve ::
    -- NOTE \source args context -> { source, args, context }
        Fn3
          (source)
          noArg
          noContext -- TODO add Context
          (Promise nOutput)
         -- { source :: source } -> Effect (Promise (Record nTargetScalars))
    )
  where
    toRelationalObjectFieldNoArg _ _ _ _ resolveFn depFn =
      { "type": toRelationalObjectHandleDepList (Proxy :: Proxy typ) depFn
      , resolve:
          mkFn3 \source _ _ ->
            -- NOTE Aff to Promise
                unsafePerformEffect
            <<< fromAff
            <<< map fromMaybeToNullable
            <<< resolveFn
            $ { source }
      }

class ToRelationalObjectFieldWithArgs
  (path :: Symbol) (source :: Type) (i :: # Type) typ target (targetScalars :: # Type)
  resolve
  (fieldRow :: # Type)
  | source i typ target targetScalars -> resolve
  , source i typ target targetScalars -> fieldRow
  where
    toRelationalObjectFieldWithArgs
      :: SProxy path -> Proxy source -> RProxy i -> Proxy typ
         -> Proxy target -> RProxy targetScalars
      -> resolve
      -> (Unit -> Nullable(GraphQLType (Maybe target)))
      -> Record fieldRow

instance toRelationalObjectFieldWithArgsImpl ::
  ( ToInputObjectWithPath path i o mArgs
  , ToRelationalObjectFieldHandleDepList typ target gType
  , ToRelationalObjectFieldHandleOutputList typ mTargetScalars mOutput
  -- TODO solve Id and String conversion
  -- NOTE solve Nullable and Maybe conversion
  , NullableAndMaybeRec (Record nArgs) (Record mArgs)
  , NullableAndMaybe nOutput mOutput
  ) => ToRelationalObjectFieldWithArgs
        path source i typ target mTargetScalars
        ( { source :: source, args :: Record mArgs}
          -> Aff mOutput
        )
        ( "type" :: GraphQLType gType
        , args :: Record o
        , resolve ::
        -- NOTE \source args context -> { source, args, context }
            Fn3
              (source)
              (Record nArgs)
              noContext -- TODO add Context
              (Promise nOutput)
        -- { source :: source, args :: Record nArgs} -> Effect (Promise (Record nTargetScalars))
        )
  where
    toRelationalObjectFieldWithArgs _ _ _ _ _ _ resolveFn depFn =
      { "type": toRelationalObjectHandleDepList (Proxy :: Proxy typ) depFn
      , args: toInputObjectWithPath
                (SProxy :: SProxy path)
                (RProxy :: RProxy i)
      , resolve:
          mkFn3 \source args _ ->
            -- NOTE Aff to Promise
                unsafePerformEffect
            <<< fromAff
            <<< map fromMaybeToNullable
            <<< resolveFn
            $ { source
              , args: fromNullableToMaybeRec args
              }
      }

---- | ToRelationalObjectFieldHandleDepList
class ToRelationalObjectFieldHandleDepList i target o | i target -> o where
  toRelationalObjectHandleDepList :: Proxy i -> (Unit -> Nullable (GraphQLType (Maybe target))) -> GraphQLType o

instance toRelationalObjectFieldHandleDepListIsListPred ::
  ( IsListPred i isList
  , ToRelationalObjectFieldHandleDepListDispatch isList i target o
  ) => ToRelationalObjectFieldHandleDepList i target o
  where
    toRelationalObjectHandleDepList _ depFn
      = toRelationalObjectFieldHandleDepListDispatch
          (BProxy :: BProxy isList)
          (Proxy :: Proxy i)
          depFn

class ToRelationalObjectFieldHandleDepListDispatch
  (isList :: Bool.Boolean) i target o
  | isList i target -> o
  where
    toRelationalObjectFieldHandleDepListDispatch
      :: BProxy isList -> Proxy i -> (Unit -> Nullable (GraphQLType (Maybe target))) -> GraphQLType o

instance toRelationalObjectFieldHandleDepListIsList ::
  ( ToRelationalObjectFieldHandleDepList a target restO
  , IsList f restO
  ) => ToRelationalObjectFieldHandleDepListDispatch
    Bool.True (f a) target (f restO)
  where
    toRelationalObjectFieldHandleDepListDispatch _ _ depFn
      = toList
        ( toRelationalObjectHandleDepList
            (Proxy :: Proxy a)
            depFn
        )

instance toRelationalObjectFieldHandleDepListNotListMaybe ::
  ToRelationalObjectFieldHandleDepListDispatch
    Bool.False (Maybe target) target (Maybe target) -- NOTE replace target by (Nullable (GraphQLType (Maybe target)))
  where
    toRelationalObjectFieldHandleDepListDispatch _ _ depFn
      = unsafeCoerce (depFn unit) -- HACK unsafely drop Nullable
else instance toRelationalObjectFieldHandleDepListNotListNotMaybe ::
  ToRelationalObjectFieldHandleDepListDispatch
    Bool.False target target target -- NOTE replace target by (Nullable (GraphQLType (Maybe target)))
  where
    toRelationalObjectFieldHandleDepListDispatch _ _ depFn
      = nonNull (unsafeCoerce (depFn unit)) -- HACK unsafely drop Nullable

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
instance toRelationalObjectFieldHandleOutputListNotListMaybe ::
  ToRelationalObjectFieldHandleOutputListDispatch
    Bool.False (Maybe a) targetScalars (Maybe (Record targetScalars)) -- NOTE replace a by targetScalars
else instance toRelationalObjectFieldHandleOutputListNotListNotMaybe ::
  ToRelationalObjectFieldHandleOutputListDispatch
  Bool.False a targetScalars (Record targetScalars) -- NOTE replace a by targetScalars

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
else instance parseListRelationalMaybeTarget ::
  ( ParseList a fieldType target
  ) => ParseList (Maybe a) fieldType target
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
instance toFieldListCons ::
  ( ToFieldList restSpecRl restFieldList
  , ParseFieldSpec fieldSpec args typ fieldType target
  ) => ToFieldList (RowList.Cons fieldName fieldSpec restSpecRl) (List.Cons (Field fieldName args typ fieldType target) restFieldList)

-- | FetchScalarFields
class FetchScalarFields (fieldList :: List.List) (scalarFields :: # Type)
  | fieldList -> scalarFields

instance fetchScalarFieldsNil ::
  FetchScalarFields List.Nil ()

instance fetchScalarFieldsConsIsScalarId :: -- NOTE Id to String
  ( FetchScalarFields restList restFields
  , Row.Cons name String restFields fields
  ) => FetchScalarFields (List.Cons (Field name argType Id ScalarField target) restList) fields
else instance fetchScalarFieldsConsIsScalarMaybeId :: -- NOTE Id to String
  ( FetchScalarFields restList restFields
  , Row.Cons name (Maybe String) restFields fields
  ) => FetchScalarFields (List.Cons (Field name argType (Maybe Id) ScalarField target) restList) fields
else instance fetchScalarFieldsConsIsScalar ::
  ( FetchScalarFields restList restFields
  , Row.Cons name typ restFields fields
  ) => FetchScalarFields (List.Cons (Field name argType typ ScalarField target) restList) fields

instance fetchScalarFieldsConsNotScalar ::
  ( FetchScalarFields restList fields
  ) => FetchScalarFields (List.Cons (Field name argType typ RelationalField target) restList) fields

