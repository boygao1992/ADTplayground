module GraphQL.TypeToField where

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

