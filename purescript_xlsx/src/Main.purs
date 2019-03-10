module Main where

import Prelude

import Data.Foldable (for_)
import Data.Traversable (for)
import Data.Lens.Index.Recordable (toRecord)
import Data.Map as Map
import Effect (Effect)
import Effect.Console (logShow)
import Node.Encoding (Encoding(Base64))
import Node.FS.Sync (readTextFile) as FS
import Text.Parsing.CSV (defaultParsers)
import Text.Parsing.Parser (runParser)
import XLSX (toCSV)

-- data ColumnType
--   = AttributeColumn
--   | CategoryColumn
--   | ShortDescriptionColumn

-- data AttributeType
--   = StaticAttribute
--   | BooleanAttribute
--   | NumberAttribute
--   | StringAttribute

type TableSchema =
  { "SKU#" :: String
  , "Product Name" :: String
  , "EAN" :: String
  , "Duty" :: Int
  }

main :: Effect Unit
main = do
  xlsx <- FS.readTextFile Base64 inputFilePath
  eCSV <- toCSV xlsx

  -- csv :: Map String String
  for_ eCSV \csv -> do
    -- csv_table :: String
    for_ (Map.lookup tableName csv) \csv_table -> do
      -- table :: List (Map String String)
      for_ (runParser csv_table defaultParsers.fileHeaded) \table -> do
        -- row :: Map String String
        for table \row -> do
          logShow $ toRecord row :: TableSchema
    where
      inputFilePath = "./source/product-feed.xlsx"
      tableName = "Sheet1"

