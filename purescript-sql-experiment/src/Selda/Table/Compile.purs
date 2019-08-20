module Selda.Table.Compile where

import Prelude

import Data.Array ((:), (!!))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)

import Selda.SQL (Param)
import Selda.SQL.Print.Config (PPConfig(..))
import Selda.SqlType (SqlTypeRep(..))
import Selda.Table (ColAttr(..), ColInfo, IndexMethod, Table(..), isAutoPrimary)
import Selda.Table.Validation (validateOrThrow)
import Selda.Types (ColName, TableName, addColPrefix, fromColName, fromTableName, intercalateColNames, modColName, rawTableName)

data OnError = Fail | Ignore
derive instance eqOnError :: Eq OnError
derive instance ordOnError :: Ord OnError
derive instance genericOnError :: Generic OnError _
instance showOnError :: Show OnError where show = genericShow

-- | Compile a sequence of queries to create the given table, including indexes.
--   The first query in the sequence is always @CREATE TABLE@.
compileCreateTable :: forall a. PPConfig -> OnError -> Table a -> String
compileCreateTable cfg ifex (Table tbl) =
  -- TODO whats the point of those unsafe operators
  -- NOTE ensureValid `seq` createTable
  unsafePerformEffect $
    pure ensureValid $> createTable
  where
    createTable = Array.fold
      [ "CREATE TABLE ", ifNotExists ifex, fromTableName tbl.tableName, "("
      , Array.intercalate ", "
          (map (compileTableCol cfg) tbl.tableCols <> multiUniques <> multiPrimary)
      , case allFKs of
          [] -> ""
          _  -> ", " <> Array.intercalate ", " compFKs
      , ")"
      ]
    multiPrimary = tbl.tableAttrs >>= case _ of
      (ixs /\ Primary) -> pure $
        Array.fold ["PRIMARY KEY(", Array.intercalate ", " (colNames ixs), ")"]
      _ -> []
    multiUniques = tbl.tableAttrs >>= case _ of
      (ixs /\ Unique) -> pure $
        Array.fold ["UNIQUE(", Array.intercalate ", " (colNames ixs), ")"]
      _ -> []
    colNames ixs = do
      ix <- ixs
      pure
        $ fromColName
        $ _.colName
        $ unsafePartial -- NOTE
        $ tbl.tableCols `Array.unsafeIndex` ix
    ifNotExists Fail   = ""
    ifNotExists Ignore = "IF NOT EXISTS "
    allFKs = do
      ci <- tbl.tableCols
      fk <- ci.colFKs
      pure $ (ci.colName /\ fk)
    compFKs = Array.mapWithIndex compileFK allFKs
    ensureValid = validateOrThrow tbl.tableName tbl.tableCols

-- | Compile the @CREATE INDEX@ queries for all indexes on the given table.
compileCreateIndexes :: forall a. PPConfig -> OnError -> Table a -> Array String
compileCreateIndexes cfg ifex (Table tbl) = tbl.tableAttrs >>= case _ of
  (idxs /\ Indexed mmethod) -> pure $
    compileCreateIndex cfg ifex tbl.tableName (colNameOfIdx <$> idxs) mmethod
  _ -> []
  where
    idxMap :: Array ColName
    idxMap = (_.colName <$> tbl.tableCols)

    colNameOfIdx :: Int -> ColName
    colNameOfIdx colIdx =
      case idxMap !! colIdx of
        Nothing   -> unsafeThrow "Impossible: Index has non-existant column-index."
        Just name -> name

-- | Get the name to use for an index on the given column(s) in the given table.
--
-- To ensure uniqueness
--
-- 1. Name multi-column indexes by connecting column names
--    with underscores.
-- 2. Escape underscores in column names.
--
-- Thus the index of columns @["foo","bar"]@ becomes @ixTable_foo_bar@ while
-- the index @["foo_bar"]@ receives an extra underscore to become
-- @ixTable_foo__bar@.
indexNameFor :: TableName -> Array ColName -> String
indexNameFor t cs =
  let
    escUnderscore c = modColName c
      (String.replaceAll (String.Pattern "_") (String.Replacement "__"))
    ixPrefix partial = "ix" <> rawTableName t <> "_" <> partial
  in
    ixPrefix (intercalateColNames "_" (escUnderscore <$> cs))

-- | Compile a @CREATE INDEX@ query for the given index.
compileCreateIndex
  :: PPConfig
  -> OnError
  -> TableName
  -> Array ColName
  -> Maybe IndexMethod
  -> String
compileCreateIndex (PPConfig cfg) ifex tbl cols mmethod = Array.fold
  [ "CREATE INDEX ", indexNameFor tbl cols, " ON ", fromTableName tbl
  , case mmethod of
      Just method -> " " <> cfg.ppIndexMethodHook method
      Nothing     -> ""
  , " (", Array.intercalate ", " (map fromColName cols), ")"
  , if ifex == Ignore then " IF NOT EXISTS" else ""
  ]

-- | Compile a foreign key constraint.
compileFK :: Int -> (ColName /\ Table Unit /\ ColName) -> String
compileFK n (col /\ Table { tableName: ftbl } /\ fcol) = Array.fold
  [ "CONSTRAINT ", fkName, " FOREIGN KEY (", fromColName col, ") "
  , "REFERENCES ", fromTableName ftbl, "(", fromColName fcol, ")"
  ]
  where
    fkName = fromColName $ addColPrefix col ("fk" <> show n <> "_")

-- | Compile a table column.
compileTableCol :: PPConfig -> ColInfo -> String
compileTableCol (PPConfig cfg) ci = Array.fold
  [ fromColName ci.colName
  , typeHook <> " " <> colAttrsHook
  ]
  where
    cty = ci.colType
    attrs = ci.colAttrs

    typeHook = cfg.ppTypeHook cty attrs
      if cty == TRowID && Array.any isAutoPrimary attrs
        then cfg.ppTypePK
        else cfg.ppType
    colAttrsHook = cfg.ppColAttrsHook cty attrs cfg.ppColAttrs

-- | Compile a @DROP TABLE@ query.
compileDropTable :: forall a. OnError -> Table a -> String
compileDropTable Fail (Table t) =
  Array.fold ["DROP TABLE",fromTableName t.tableName]
compileDropTable _ (Table t) =
  Array.fold ["DROP TABLE IF EXISTS",fromTableName t.tableName]

-- | Compile an @INSERT INTO@ query inserting @m@ rows with @n@ cols each.
--   Note that backends expect insertions to NOT have a semicolon at the end.
--   In addition to the compiled query, this function also returns the list of
--   parameters to be passed to the backend.
compInsert
  :: forall a
  . PPConfig
  -> Table a
  -> Array (Array (Either Param Param))
  -> (String /\ Array Param)
compInsert (PPConfig cfg) (Table tbl) defs =
  let (vals /\ parameters) = mkRows 1 defs [] []
      values = Array.intercalate ", " vals
      colNames = map _.colName $ tbl.tableCols
      query = Array.fold
        [ "INSERT INTO"
        , fromTableName tbl.tableName
        , "(" <>  Array.intercalate ", " (map fromColName colNames) <> ")"
        , "VALUES"
        , values
        ]
  in (query /\ parameters)
  where
    -- Build all rows: just recurse over the list of defaults (which encodes
    -- the # of elements in total as well), building each row, keeping track
    -- of the next parameter identifier.
    mkRows n psss rts paramss = case Array.uncons psss of
      Nothing ->
        (Array.reverse rts /\ (Array.reverse $ Array.concat paramss))
      Just {head: ps, tail: pss} ->
        let
          (n' /\ names /\ params) = mkRow n ps tbl.tableCols
          rowText = "(" <> Array.intercalate ", " (Array.reverse names) <> ")"
        in
          mkRows n' pss (rowText:rts) (params:paramss)
    -- Build a row: use the NULL/DEFAULT keyword for default rows, otherwise
    -- use a parameter.
    mkRow n ps names = Array.foldl mkCols (n /\ [] /\ []) (Array.zip ps names)

    -- Create a colum and return the next parameter id, plus the column itself.
    mkCols
      :: (Int /\ Array String /\ Array Param)
      -> (Either Param Param /\ ColInfo) -> (Int /\ Array String /\ Array Param)
    mkCols (n /\ names /\ params) (param /\ col) =
      case mkCol n param col params of
        (n' /\ name /\ params') -> (n' /\ (name:names) /\ params')
    -- Build a column: default values only available for for auto-incrementing
    -- primary keys.
    mkCol
      :: Int
      -> Either Param Param
      -> ColInfo
      -> Array Param
      -> (Int /\ String /\ Array Param)
    mkCol n (Left def) col ps
      | Array.any isAutoPrimary col.colAttrs =
        (n /\ cfg.ppAutoIncInsert /\ ps)
      | otherwise =
          ((n+1) /\ ("$" <> show n) /\ def:ps)
    mkCol n (Right val) _ ps =
      ((n+1) /\ ("$" <> show n) /\ val:ps)

