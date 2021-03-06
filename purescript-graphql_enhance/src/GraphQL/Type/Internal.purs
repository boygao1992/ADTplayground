module GraphQL.Type.Internal where

import Data.Argonaut.Core (Json)
import Data.Function.Uncurried (Fn1, Fn6, runFn1, runFn6)
import Data.Maybe (Maybe)
import Data.NonEmpty (NonEmpty)
import Data.Nullable (Nullable, toNullable)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Prelude
import Prim.TypeError (class Fail, Beside, Quote, Text)
import Type.Data.Boolean as Bool

{-
data ObjectType a
data InputObjectType a
data ScalarType a
data ListType (f :: Type -> Type) a

class GraphQLType a

instance graphQLTypeScalarType :: GraphQLType (ScalarType a)
instance graphQLTypeListType :: GraphQLType (List f a)
-}


data GraphQLType spec

data GraphQLRootType rootSpec rootSource

data Schema rootSpec rootSource

newtype Id = Id String

-- | FFI

foreign import int :: GraphQLType (Maybe Int)
foreign import float :: GraphQLType (Maybe Number)
foreign import string :: GraphQLType (Maybe String)
foreign import id :: GraphQLType (Maybe Id)
foreign import boolean :: GraphQLType (Maybe Boolean)

foreign import _nonNull ::
  forall a. Fn1 (GraphQLType (Maybe a)) (GraphQLType a)

nonNull :: forall a. GraphQLType (Maybe a) -> GraphQLType a
nonNull gType = runFn1 _nonNull gType

foreign import _list ::
  forall f a. Fn1 (GraphQLType a) (GraphQLType (Maybe (f a)))

list :: forall f a. GraphQLType a -> GraphQLType (Maybe (f a))
list gType = runFn1 _list gType

foreign import _inputObjectType :: forall row typ. Fn1 (Record row) (GraphQLType (Maybe typ))

inputObjectType :: forall row typ. Record row -> GraphQLType (Maybe typ)
inputObjectType config = runFn1 _inputObjectType config

foreign import _objectType :: forall row g typ. Fn1 (Record row) (g typ)

objectType :: forall row typ. Record row -> GraphQLType (Maybe typ)
objectType config = runFn1 _objectType config

rootObjectType :: forall row rootSpec rootSource. Record row -> GraphQLRootType rootSpec rootSource
rootObjectType config = runFn1 _objectType config

foreign import _schema :: forall rootSpec rootSource. Fn1 (GraphQLRootType rootSpec rootSource)  (Schema rootSpec rootSource)

schema
  :: forall rootSpec rootSource
   . GraphQLRootType rootSpec rootSource
  -> Schema rootSpec rootSource
schema root = runFn1 _schema root

-- NOTE from purescript-graphql
foreign import _graphql :: forall rootSpec rootSource ctx.
  Fn6 (Schema rootSpec rootSource) String rootSource ctx (Nullable Json) (Nullable String) (EffectFnAff Json)
-- NOTE from purescript-graphql
graphql
  :: forall rootSpec rootSource ctx
   . Schema rootSpec rootSource -> String
  -> rootSource -> ctx -> Maybe Json -> Maybe String -> Aff Json
graphql s query root context variables operationName =
  fromEffectFnAff $ runFn6 _graphql s query root context nVariables nOperation
    where
      nVariables = toNullable variables
      nOperation = toNullable operationName

-- | IsUnitPred
-- NOTE output type can be Unit for mutations
-- NOTE GraphQL doesn't support Unit type
--   https://github.com/apollographql/graphql-tools/issues/277
class IsUnitPred a (b :: Bool.Boolean) | a -> b
instance isUnitPredYes :: IsUnitPred Unit Bool.True
else instance isUnitPredNo :: IsUnitPred other Bool.False

-- | IsScalarPred
class IsScalarPred a (b :: Bool.Boolean) | a -> b
instance isScalarPredInt ::
  IsScalarPred Int Bool.True
else instance isScalarPredNumber ::
  IsScalarPred Number Bool.True
else instance isScalarPredString ::
  IsScalarPred String Bool.True
else instance isScalarPredId ::
  IsScalarPred Id Bool.True
else instance isScalarPredBoolean ::
  IsScalarPred Boolean Bool.True
else instance isScalarPredMaybeInt ::
  IsScalarPred (Maybe Int) Bool.True
else instance isScalarPredMaybeNumber ::
  IsScalarPred (Maybe Number) Bool.True
else instance isScalarPredMaybeString ::
  IsScalarPred (Maybe String) Bool.True
else instance isScalarPredMaybeId ::
  IsScalarPred (Maybe Id) Bool.True
else instance isScalarPredMaybeBoolean ::
  IsScalarPred (Maybe Boolean) Bool.True
else instance isScalarPredNo ::
  IsScalarPred other Bool.False

-- | IsScalar
class IsScalar a where
  toScalar :: GraphQLType a

instance isScalarInt :: IsScalar Int where
  toScalar = nonNull int
else instance isScalarNumber :: IsScalar Number where
  toScalar = nonNull float
else instance isScalarString :: IsScalar String where
  toScalar = nonNull string
else instance isScalarId :: IsScalar Id where
  toScalar = nonNull id
else instance isScalarBoolean :: IsScalar Boolean where
  toScalar = nonNull boolean
else instance isScalarMaybeInt :: IsScalar (Maybe Int) where
  toScalar = int
else instance isScalarMaybeNumber :: IsScalar (Maybe Number) where
  toScalar = float
else instance isScalarMaybeString :: IsScalar (Maybe String) where
  toScalar = string
else instance isScalarMaybeId :: IsScalar (Maybe Id) where
  toScalar = id
else instance isScalarMaybeBoolean :: IsScalar (Maybe Boolean) where
  toScalar = boolean
else instance isScalarNo ::
  Fail
  ( Beside
    ( Quote a)
    ( Text " is not a valid scalar type.")
  )
  => IsScalar a where
  toScalar = toScalar

-- | IsListPred
class IsListPred a (b :: Bool.Boolean) | a -> b
instance isListPredArray ::
  IsListPred (Array a) Bool.True
else instance isListPredNonEmptyArray ::
  IsListPred (NonEmpty Array a) Bool.True
else instance isListPredNo ::
  IsListPred other Bool.False

-- | IsList
class IsList (f :: Type -> Type) (a :: Type) where
  toList :: GraphQLType a -> GraphQLType (f a)

instance isListArray :: IsList Array a where
  toList gType = nonNull (list gType)
else instance isListNonEmpty :: IsList (NonEmpty Array) a where
  toList gType = nonNull (list gType)
else instance isListNo ::
  Fail
  ( Beside
    (Quote (f a))
    (Text " is not a valid list type.")
  )
  => IsList f a where
  toList = toList
