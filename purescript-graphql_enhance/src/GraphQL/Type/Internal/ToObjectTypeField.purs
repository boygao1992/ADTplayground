module GraphQL.Type.Internal.ToObjectTypeField where

import Prelude

import Prim.RowList (kind RowList)
import Data.NonEmpty (NonEmpty)
import GraphQL.Type.Internal (class IsScalarPred, GraphQLType)
import Data.Nullable (Nullable)
import Prim.RowList as RowList
import Type.Data.Boolean as Bool
import Type.Data.List as List
import Type.Data.Symbol (SProxy)
import Type.Data.Symbol as Symbol
import Type.Proxy (Proxy)


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

class ToRelationalObjectField
  (path :: Symbol) source fieldSpec -- input
  resolve target fieldRow -- output
  | source fieldSpec -> resolve
  , fieldSpec -> target
  , path source fieldSpec -> fieldRow
  where
    toRelationalObjectField
      :: SProxy path -> Proxy source -> Proxy fieldSpec
      -> resolve
      -> (Unit -> Nullable(GraphQLType target))
      -> (Record fieldRow)

-- TODO
-- instance toRelationalObjectFieldImpl ::
--   ( ParseFieldSpec fieldSpec argType typ -- fieldSpec -> args typ
--   , ToFieldType typ fieldType target -- typ -> fieldType target
--   -- , ToInputObjectWithPath path i NOTE if argType == (WithArgs i)
--   -- , UnwrapNewtype i args
--   ) => ToRelationalObjectField path source fieldSpec resolve target fieldRow

-- | FieldType = ScalarField | RelationalField
foreign import kind FieldType
foreign import data ScalarField :: FieldType
foreign import data RelationalField :: FieldType

data FTProxy (fieldType :: FieldType) = FTProxy

-- | Args = NoArgs | WithArgs a
foreign import kind ArgType
foreign import data NoArg :: ArgType
foreign import data WithArgs :: Type -> ArgType

data AProxy (args :: ArgType) = AProxy

-- | Field
data Field (name :: Symbol) (target :: Type) (fieldType :: FieldType) (args :: ArgType)

-- | ObjectType

data ObjectType a -- a = Newtype (Record (spec :: # Type))
data ObjectTypeField

-- | ParseFieldSpec
class ParseFieldSpec (spec :: Type) (args :: ArgType) typ | spec -> args typ

instance parseFieldSpecWithArgs ::
  ParseFieldSpec (i -> o) (WithArgs i) o
else instance parseFieldSpecNoArg ::
  ParseFieldSpec o NoArg o

-- | ParseList
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

-- | ToFieldType
class ToFieldType a (fieldType :: FieldType) | a -> fieldType

instance toFieldTypeImpl ::
  ( IsScalarPred a isScalar
  , Bool.If isScalar
      (FTProxy ScalarField)
      (FTProxy RelationalField)
      (FTProxy fieldType)
  ) => ToFieldType a fieldType
