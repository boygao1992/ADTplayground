module Selda.Generic where

import Prelude
import Type.Prelude

import Control.Monad.State
import Effect
import Data.Maybe
import Data.Either
import Data.Variant.Internal

import Selda.Types
import Selda.SqlType
import Selda.SqlRow (class SqlRow)
import Selda.Table.Type
import Selda.SQL (Param (..))
import Selda.Exp (Exp (Col, Lit), UntypedCol (..))

-- | Any type which has a corresponding relation.
--   To make a @Relational@ instance for some type, simply derive 'Generic'.
--
--   Note that only types which have a single data constructor, and where all
--   fields are instances of 'SqlValue' can be used with this module.
--   Attempting to use functions in this module with any type which doesn't
--   obey those constraints will result in a very confusing type error.
-- type Relational a =
--   ( Generic a
--   , SqlRow a
--   , GRelation (Rep a)
--   )
-- NOTE missing constraint kind

-- | Extract all insert parameters from a generic value.
-- TODO params :: Relational a => a -> [Either Param Param]


-- | Extract all column names from the given type.
--   If the type is not a record, the columns will be named @col_1@,
--   @col_2@, etc.
-- TODO tblCols :: forall a. Relational a => Proxy a -> (Text -> Text) -> [ColInfo]

-- | Exception indicating the use of a default value.
--   If any values throwing this during evaluation of @param xs@ will be
--   replaced by their default value.
-- data DefaultValueException = DefaultValueException
-- instance Exception DefaultValueException
-- NOTE missing Exception hierarchy

class GRelation f where
  -- | Generic worker for 'params'.
  gParams :: forall a. f a -> Effect (Array (Either Param Param))

  -- | Compute all columns needed to represent the given type.
  gTblCols
    :: FProxy f
    -> Maybe ColName
    -> (Int -> Maybe ColName -> ColName)
    -> State Int (Array ColInfo)

  -- | Create a new value with all default fields.
  gNew :: forall sql. FProxy f -> Array (UntypedCol sql)

-- TODO GRelation instances
