module Selda.SQL.Print.Config where

import Data.Maybe (Maybe)

import Selda.SqlType (SqlTypeRep(..))
import Selda.Table (AutoIncType(..), ColAttr(..), IndexMethod)

-- | Backend-specific configuration for the SQL pretty-printer.
newtype PPConfig = PPConfig
  { -- | The SQL type name of the given type.
    --
    --   This function should be used everywhere a type is needed to be printed but in primary
    --   keys position. This is due to the fact that some backends might have a special
    --   representation of primary keys (using sequences are such). If you have such a need,
    --   please use the 'ppTypePK' record instead.
    ppType :: SqlTypeRep -> String

    -- | Hook that allows you to modify 'ppType' output.
  , ppTypeHook :: SqlTypeRep -> Array ColAttr -> (SqlTypeRep -> String) -> String

    -- | The SQL type name of the given type for primary keys uses.
  , ppTypePK :: SqlTypeRep -> String

    -- | Parameter placeholder for the @n@th parameter.
  , ppPlaceholder :: Int -> String

    -- | List of column attributes.
  , ppColAttrs :: Array ColAttr -> String

    -- | Hook that allows you to modify 'ppColAttrs' output.
  , ppColAttrsHook :: SqlTypeRep -> Array ColAttr -> (Array ColAttr -> String) -> String

    -- | The value used for the next value for an auto-incrementing column.
    --   For instance, @DEFAULT@ for PostgreSQL, and @NULL@ for SQLite.
  , ppAutoIncInsert :: String

    -- | Insert queries may have at most this many parameters; if an insertion
    --   has more parameters than this, it will be chunked.
    --
    --   Note that only insertions of multiple rows are chunked. If your table
    --   has more than this many columns, you should really rethink
    --   your database design.
  , ppMaxInsertParams :: Maybe Int

    -- | @CREATE INDEX@ suffix to indicate that the index should use the given
    --   index method.
  , ppIndexMethodHook :: IndexMethod -> String
  }

-- | Default settings for pretty-printing.
--   Geared towards SQLite.
--
--   The default definition of 'ppTypePK' is 'defType, so that you don’t have to do anything
--   special if you don’t use special types for primary keys.
-- TODO defPPConfig :: PPConfig


-- | Default compilation for SQL types.
--   By default, anything we don't know is just a blob.
defType :: SqlTypeRep -> String
defType TText     = "TEXT"
defType TRowID    = "INTEGER"
defType TInt      = "INT"
defType TFloat    = "DOUBLE"
defType TBool     = "BOOLEAN"
defType TDateTime = "DATETIME"
defType TDate     = "DATE"
defType TTime     = "TIME"
defType TBlob     = "BLOB"
defType TUUID     = "BLOB"
defType TJSON     = "BLOB"

-- | Default compilation for a column attribute.
defColAttr :: ColAttr -> String
defColAttr Primary              = ""
defColAttr (AutoPrimary Strong) = "PRIMARY KEY AUTOINCREMENT"
defColAttr (AutoPrimary Weak)   = "PRIMARY KEY"
defColAttr Required             = "NOT NULL"
defColAttr Optional             = "NULL"
defColAttr Unique               = "UNIQUE"
defColAttr (Indexed _)          = ""
