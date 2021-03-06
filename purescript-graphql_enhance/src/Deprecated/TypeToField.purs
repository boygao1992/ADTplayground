module Deprecated.TypeToField where


-- NOTE
-- assumptions to simplify API
-- 1. output of resolver will fulfill all scalars of childType


import Prelude (Void)

import Type.Data.Boolean as Bool
import Type.Row.Utils (class HasFieldPred)

-- | ParseFieldType
-- | example1: with args
-- Query { posts :: { id :: Id } -> Array Post }
--   i = { id :: Id }
--   o = Array Post
-- | example2: without args
-- User { name :: String }
--   i = Void
--   o = String

class ParseFieldType f i o | f -> i o

instance parseFieldTypeIsFunction ::
  ParseFieldType (i -> o) i o
else instance parseFieldTypeNotFunction ::
  ParseFieldType o Void o

class GraphQLObjectField i o (missing :: # Type)

{-
from parent
- sourceType (parent's outputType) NOTE removed by assumption1
- context (carried all the way down from root node)

input variables
- scalars
- relations
- args

type variables
- fieldName :: Symbol
  - e.g. "id"
- fieldType
  -> argsType outputType
- fieldRow :: # Type
  - "description"

resolveFnType ::
  { source? :: sourceType
  , args? :: argsType
  , context? :: contextType
  } -> Aff outputType

missing fields =
{ description? :: String
, fields :
  { posts :
    { description? :: String
    , resolve :: resolveFnType
    }
  }
}


class ToGraphQLObjectTypeMissingField psType source ctx output (missing :: # Type) | psType source ctx output -> missing

-}

-- | IsObjectLabel
class IsObjectLabel (label :: Symbol)
instance isObjectLabelDescription :: IsObjectLabel "description"
instance isObjectLabelFields :: IsObjectLabel "fields"

-- | IsObjectFieldLabel
class IsObjectFieldLabel (label :: Symbol)
instance isObjectFieldLabelDescription :: IsObjectFieldLabel "description"
instance isObjectFieldLabelArgs :: IsObjectFieldLabel "args"
instance isObjectFieldLabelResolve :: IsObjectFieldLabel "resolve"

-- | Description
class HasDescription (row :: # Type) (b :: Bool.Boolean) | row -> b

instance hasDescriptionImpl ::
  ( HasFieldPred row "description" b
  ) => HasDescription row b

-- | Resolver

-- | Source
class HasSource (row :: # Type) (b :: Bool.Boolean) | row -> b

instance hasSourceImpl ::
  ( HasFieldPred row "source" b
  ) => HasSource row b

-- | BoundedSource

-- | Args
class HasArgs (row :: # Type) (b :: Bool.Boolean) | row -> b

instance hasArgsImpl ::
  ( HasFieldPred row "args" b
  ) => HasArgs row b

-- | Context
class HasContext (row :: # Type) (b :: Bool.Boolean) | row -> b

instance hasContextImpl ::
  ( HasFieldPred row "context" b
  ) => HasContext row b

