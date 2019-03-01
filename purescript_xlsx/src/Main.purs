module Main where

import Prelude

import Data.Foldable (for_)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(Base64))
import Node.FS.Sync (readTextFile) as FS
import Text.Parsing.CSV (defaultParsers)
import Text.Parsing.Parser (runParser)
import Utils.XLSX (toCSV)
import Data.Map as Map
import Data.List as List


main :: Effect Unit
main = do
  xlsx <- FS.readTextFile Base64 inputFilePath
  eCSV <- toCSV xlsx tableName
  for_ eCSV \csv -> do
    for_ (runParser csv defaultParsers.fileHeaded) \parsed -> do
      for_ (Map.lookup "SKU#" =<< List.head parsed) \value -> do
        log value

  where
    inputFilePath = "./source/product-feed.xlsx"
    tableName = "Sheet1"
