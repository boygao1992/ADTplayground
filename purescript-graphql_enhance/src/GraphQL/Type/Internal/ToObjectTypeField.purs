module GraphQL.Type.Internal.ToObjectTypeField where

import Prelude

import Data.Nullable (Nullable)
import Effect.Aff (Aff)
import GraphQL.Type.Internal (GraphQLType)
import Prim.RowList as RowList
import Type.Data.Boolean as Bool
import Type.Data.Symbol (SProxy)
import Type.Data.Symbol as Symbol
import Type.Proxy (Proxy)

-- NOTE refactoring of ValidateMissingFields with better organized constructs
-- currently hard to implement value-level transformations

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
