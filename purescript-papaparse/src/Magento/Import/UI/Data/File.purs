module Magento.Import.UI.Data.File where

import Prelude

import Data.Argonaut.Encode (class EncodeJson)
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Magento.Import.Data.Skus (Sku(..))
import Magento.Import.Data.Skus (columnName) as Skus

newtype File = File (Array (Array String))
derive instance newtypeFile :: Newtype File _
derive newtype instance showFile :: Show File

newtype RowTable = RowTable (Array (Map String String))
derive instance newtypeRowTable :: Newtype RowTable _
derive newtype instance showRowTable :: Show RowTable
derive newtype instance semigroupRowTable :: Semigroup RowTable
derive newtype instance monoidRowTable :: Monoid RowTable

newtype Column = Column (Array (Tuple Sku String))
derive newtype instance encodeJsonColumn :: EncodeJson Column
derive newtype instance showColumn :: Show Column
derive newtype instance semigroupColumn :: Semigroup Column
derive newtype instance monoidColumn :: Monoid Column
derive instance newtypeColumn :: Newtype Column _

newtype ColumnTable = ColumnTable (Map String Column)
derive instance newtypeColumnTable :: Newtype ColumnTable _
derive newtype instance showColumnTable :: Show ColumnTable
derive newtype instance semigroupColumnTable :: Semigroup ColumnTable
derive newtype instance monoidColumnTable :: Monoid ColumnTable


parseRowTable :: File -> Maybe RowTable
parseRowTable (File file) = case Array.uncons file of
  Nothing -> Nothing
  Just { head: header, tail: rows } ->
    Just $ RowTable $ (Map.fromFoldable <<< Array.zip header) <$> rows

parseSingleColumn :: String -> RowTable -> Maybe Column
parseSingleColumn columnName (RowTable rows) =
  Column
  <$> for rows \row ->
    Tuple
    <$> (Sku <$> Map.lookup Skus.columnName row)
    <*> Map.lookup columnName row

{-
header :: Array String
rows :: Array (Array String)

newtype Header = Header (Row)
newtype Rows = Rows (Array Row)
newtype Row = Row (Array String)



-}

-- parseColumnTable :: File -> Maybe ColumnTable
-- parseColumnTable (File file) = case Array.uncons file of
--   Nothing -> Nothing
--   Just { head: header, tail: rows } ->

fetchColumnByName :: ColumnTable -> String -> Maybe Column
fetchColumnByName (ColumnTable columnDict) columnName =
  Map.lookup columnName columnDict

