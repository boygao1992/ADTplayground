module Main where

import Prelude

import Data.Foldable (for_)
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(Base64))
import Node.FS.Sync (readTextFile) as FS
import Text.Parsing.CSV (defaultParsers)
import Text.Parsing.Parser (runParser)
import Utils.XLSX (toCSV)
import Data.Map as Map
import Data.List as List

data ColumnType
  = AttributeColumn
  | CategoryColumn
  | ShortDescriptionColumn

data AttributeType
  = StaticAttribute
  | BooleanAttribute
  | NumberAttribute
  | StringAttribute

main :: Effect Unit
main = do
  xlsx <- FS.readTextFile Base64 inputFilePath
  eCSV <- toCSV xlsx

  for_ eCSV \csv -> do
    for_ (Map.lookup tableName csv) \table -> do
      -- NOTE type Table = List (Map String String)
      -- parsed :: Table
      for_ (runParser table defaultParsers.fileHeaded) \parsed -> do
          for_ (Map.lookup columnName =<< List.head parsed) \value -> do
            log value

  where
    inputFilePath = "./source/product-feed.xlsx"
    tableName = "Sheet1"
    columnName = "SKU#"
