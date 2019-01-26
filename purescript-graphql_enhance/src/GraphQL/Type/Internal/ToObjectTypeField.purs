module GraphQL.Type.Internal.ToObjectTypeField where

import Prelude

import Data.Nullable (Nullable)
import Effect.Aff (Aff)
import GraphQL.Type.Internal (GraphQLType)
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Type.Data.Boolean as Bool
import Type.Data.Symbol (SProxy)
import Type.Data.Symbol as Symbol
import Type.Proxy (Proxy)
import Data.Generic.Rep (class Generic, Constructor, Argument)

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

-- | Args = NoArgs | WithArgs a
foreign import kind Args
foreign import data NoArg :: Args
foreign import data WithArgs :: Type -> Args

data AProxy (args :: Args) = AProxy

data ObjectType a -- a = Newtype (Record (spec :: # Type))
data ObjectTypeField

-- | ParseFieldSpec
class ParseFieldSpec (spec :: Type) (args :: Args) a | spec -> args a

instance parseFieldSpecWithArgs ::
  ParseFieldSpec (i -> o) (WithArgs i) o
else instance parseFieldSpecNoArg ::
  ParseFieldSpec o NoArg o

-- | ToObjectType
class ToObjectType i (o :: # Type) | i -> o

instance toObjectTypeImpl ::
  ( Generic i (Constructor name (Argument (Record specRow)))
  , RowList.RowToList specRow specRl
  ) => ToObjectType i o
-- Symbol.IsSymbol name =>
-- Proxy i ->
-- (Record resolvers) ->
-- (Record deps) ->
-- Record o
-- =
--   { name: Symbol.reflectSymbol name
--   , fields: toObjectRowList (RProxy :: RProxy specRow)
--   }

class ToObjectTypeRowList (specRl :: RowList) (resolvers :: # Type) (deps :: # Type) (o :: # Type) | specRl -> resolvers deps o

-- | ToObjectTypeField
class ToObjectTypeField (args :: Args) a
  where
    toObjectTypeField :: AProxy args -> Proxy a -> ObjectTypeField

--   | ToScalarObjectTypeField
type ScalarObjectTypeFieldShape typ =
  { "type"
      :: GraphQLType typ
  -- , args
  --     :: Maybe argsRecord
  -- , resolve
  --     :: Maybe (...)
  }

class ToScalarObjectTypeField (args :: Args) a
  where
    toScalarObjectField :: AProxy args -> Proxy a -> ObjectTypeField


--   | ToRelationalObjectTypeField
type RelationalObjectTypeFieldShape
  typ scalarFieldsOfTyp sourceRecord argsRecord ctx resolve =
  { "type"
      :: ObjectType typ -- TODO unify scalar and relational; currently, scalar types are under general GraphQLType
  , args
      :: argsRecord -- TODO different from argSpec which contains Newtype; need to unwrap Newtype as ToInputObject
  , resolve
      :: { source :: sourceRecord, args :: argsRecord, context :: ctx }
      -> Aff scalarFieldsOfTyp
  -- , description
  --     :: Maybe String -- NOTE omit description for now
  }

class ToRelationalObjectTypeField (args :: Args) a
  where
    toRelationalObjectFieldConstructor
      :: AProxy args -> Proxy a
      -> ( (Unit -> Nullable (ObjectType a)) -> ObjectTypeField )
