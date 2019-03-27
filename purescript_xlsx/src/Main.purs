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
import Text.Parsing.CSV.Enhance (defaultParsers')
import Text.Parsing.Parser (runParser)
import XLSX (toCSV)
import Data.List as List

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
      -- for_ (runParser csv_table defaultParsers.fileHeaded) \table -> do
      --   -- row :: Map String String
      --   for table \row -> do
      --     logShow $ toRecord row :: TableSchema

      -- table :: List (Map String (NonEmptyList String))
      for_ (runParser csv_table defaultParsers'.fileHeadedDup) \table -> do
        logShow $ Map.lookup "SKU#" =<< List.head table

    where
      inputFilePath = "./source/product-feed.xlsx"
      tableName = "Sheet1"
