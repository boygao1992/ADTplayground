module Generic.GraphQLType where

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Generic.EnumToArray
import GraphQL.Type as G
import Type.Proxy (Proxy)
import Data.Newtype (class Newtype, unwrap)

class G.GraphQLType gl <= ToGraphQLType ps gl | ps -> gl where
  toGraphQLType :: Proxy ps -> gl

-- | Primitive
instance toGraphQLTypeNumber :: ToGraphQLType Number (G.ScalarType Number)
  where
    toGraphQLType _ = G.nonNull G.float
instance toGraphQLTypeMaybeNumber :: ToGraphQLType (Maybe Number) (G.ScalarType (Maybe Number))
  where
    toGraphQLType _ = G.float

instance toGraphQLTypeInt :: ToGraphQLType Int (G.ScalarType Int)
  where
    toGraphQLType _ = G.nonNull G.int
instance toGraphQLTypeMaybeInt :: ToGraphQLType (Maybe Int) (G.ScalarType (Maybe Int))
  where
    toGraphQLType _ = G.int

instance toGraphQLTypeString :: ToGraphQLType String (G.ScalarType String)
  where
    toGraphQLType _ = G.nonNull G.string
instance toGraphQLTypeMaybeString :: ToGraphQLType (Maybe String) (G.ScalarType (Maybe String))
  where
    toGraphQLType _ = G.string

newtype Id = Id String
instance toGraphQLTypeId :: ToGraphQLType Id (G.ScalarType String)
  where
    toGraphQLType _ = G.nonNull G.id
instance toGraphQLTypeMaybeId :: ToGraphQLType (Maybe Id) (G.ScalarType (Maybe String))
  where
    toGraphQLType _ = G.id

instance toGraphQLTypeBoolean :: ToGraphQLType Boolean (G.ScalarType Boolean)
  where
    toGraphQLType _ = G.nonNull G.boolean
instance toGraphQLTypeMaybeBoolean :: ToGraphQLType (Maybe Boolean) (G.ScalarType (Maybe Boolean))
  where
    toGraphQLType _ = G.boolean

-- | Name
newtype Name a = Name String
derive instance newtypeName :: Newtype (Name a) _

class TypeName a where
  typeName :: Name a

-- | Generic
enumToGraphQLType :: forall a rep. TypeName a => Generic a rep => GenericEnumToArray rep => G.EnumType (Maybe a)
enumToGraphQLType = G.enumType (unwrap (typeName :: Name a)) Nothing enumToEnumValueArray

