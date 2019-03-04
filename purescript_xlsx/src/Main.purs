module Main where

import Prelude

import Data.Foldable (for_)
import Data.HObject.Record (hObjToRecord)
import Data.List as List
import Data.Map as Map
import Effect (Effect)
import Effect.Console (logShow)
import Node.Encoding (Encoding(Base64))
import Node.FS.Sync (readTextFile) as FS
import Text.Parsing.CSV.Utils (hObject)
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
  -- , "Duty" :: Int -- TODO cannot be parsed into Int
  }

main :: Effect Unit
main = do
  xlsx <- FS.readTextFile Base64 inputFilePath
  eCSV <- toCSV xlsx

  for_ eCSV \csv -> do
    for_ (Map.lookup tableName csv) \table -> do
      -- NOTE type Table = List (List (Tuple String String))
      -- parsed :: Table
      for_ (runParser table hObject) \parsed -> do
        for_ (List.head parsed) \firstRow -> do
          for_ (hObjToRecord firstRow) \record -> do
            logShow $ record :: TableSchema
    where
      inputFilePath = "./source/product-feed.xlsx"
      tableName = "Sheet1"
      columnName = "SKU#"

