module Generic.GraphQLType where

import Data.Generic.Rep
import Prelude

import Data.Maybe (Maybe)
import Generic.IsEnum (class IsEnum)
import GraphQL.Type as G
import Type.Proxy (Proxy)

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

-- | Generic
class GenericToGraphQLType rep gl where
  genericToGraphQLType' :: rep -> gl

