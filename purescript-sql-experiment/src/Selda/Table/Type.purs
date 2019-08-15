module Selda.Table.Type where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))

import Selda.Exp (UntypedCol)
import Selda.SQL (SQL)
import Selda.SqlType (SqlTypeRep)
import Selda.Types (ColName, TableName)

-- | A database table, based on some Haskell data type.
--   Any single constructor type can form the basis of a table, as long as
--   it derives @Generic@ and all of its fields are instances of @SqlType@.
newtype Table a = Table
  { -- | Name of the table. NOT guaranteed to be a valid SQL name.
    tableName :: TableName

    -- | All table columns.
    --   Invariant: the 'colAttrs' list of each column is sorted and contains
    --   no duplicates.
  , tableCols :: Array ColInfo

    -- | Does the given table have an auto-incrementing primary key?
  , tableHasAutoPK :: Boolean

    -- | Attributes involving multiple columns.
  , tableAttrs :: Array (Array Int /\ ColAttr)
  }


-- | A complete description of a database column.
newtype ColInfo = ColInfo
  { colName  :: ColName
  , colType  :: SqlTypeRep
  , colAttrs :: Array ColAttr
  , colFKs   :: Array (Table Unit /\ ColName)
  , colExpr  :: UntypedCol SQL
  }

-- | Get all table columns with an explicit index.
-- NOTE deprecated from 0.5.0.0
-- indexedCols :: forall a. Table a -> Array (ColName /\ Maybe IndexMethod)
-- indexedCols (Table t) = do
--   (ColInfo col) <- t.tableCols
--   colAttr <- col.colAttrs
--   let mmethod = case colAttr of
--         Indexed mmethod' -> mmethod'
--         _ -> Nothing
--   pure (col.colName /\ mmethod)

-- | Strongly or weakly auto-incrementing primary key?
data AutoIncType = Weak | Strong
derive instance eqAutoIncType :: Eq AutoIncType
derive instance ordAutoIncType :: Ord AutoIncType
derive instance genericAutoIncType :: Generic AutoIncType _
instance showAutoIncType :: Show AutoIncType where show = genericShow

-- | Column attributes such as nullability, auto increment, etc.
--   When adding elements, make sure that they are added in the order
--   required by SQL syntax, as this list is only sorted before being
--   pretty-printed.
data ColAttr
  = Primary
  | AutoPrimary AutoIncType
  | Required
  | Optional
  | Unique
  | Indexed (Maybe IndexMethod)
derive instance eqColAttr :: Eq ColAttr
derive instance ordColAttr :: Ord ColAttr
derive instance genericColAttr :: Generic ColAttr _
instance showColAttr :: Show ColAttr where show = genericShow

isAutoPrimary :: ColAttr -> Boolean
isAutoPrimary (AutoPrimary _) = true
isAutoPrimary _               = false

isPrimary :: ColAttr -> Boolean
isPrimary Primary = true
isPrimary attr    = isAutoPrimary attr

isUnique :: ColAttr -> Boolean
isUnique Unique      = true
isUnique (Indexed _) = true
isUnique attr        = isPrimary attr

-- | Method to use for indexing with 'indexedUsing'.
--   Index methods are ignored by the SQLite backend, as SQLite doesn't support
--   different index methods.
data IndexMethod
  = BTreeIndex
  | HashIndex
-- Omitted until the operator class business is sorted out
-- | GistIndex
-- | GinIndex
derive instance eqIndexMethod :: Eq IndexMethod
derive instance ordIndexMethod :: Ord IndexMethod
derive instance genericIndexMethod :: Generic IndexMethod _
instance showIndexMethod :: Show IndexMethod where show = genericShow
