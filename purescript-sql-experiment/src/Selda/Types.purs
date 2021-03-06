module Selda.Types where

import Prelude

import Data.Newtype (class Newtype, unwrap)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.String as String

--------
-- Utils

-- | Escape double quotes in an SQL identifier.
escapeQuotes :: String -> String
escapeQuotes = String.replace (String.Pattern "\"") (String.Replacement "\"\"")

--------
-- Types

newtype ColName = ColName String
derive instance newtypeColName :: Newtype ColName _
derive instance eqColName :: Eq ColName
derive instance ordColName :: Ord ColName
derive newtype instance showColName :: Show ColName

newtype TableName = TableName String
derive instance newtypeTableName :: Newtype TableName _
derive instance eqTableName :: Eq TableName
derive instance ordTableName :: Ord TableName
derive newtype instance showTableName :: Show TableName

modColName :: ColName -> (String -> String) -> ColName
modColName (ColName cn) f = ColName $ f cn

addColPrefix :: ColName -> String -> ColName
addColPrefix (ColName cn) p = ColName $ p <> cn

addColSuffix :: ColName -> String -> ColName
addColSuffix (ColName cn) s = ColName $ cn <> s

fromColName :: ColName -> String
fromColName (ColName cn) = "\"" <> escapeQuotes cn <> "\""

-- | Convert column names into a string, without quotes, intercalating the given
-- string.
--
-- @
-- intercalateColNames "_" [ColName "a", ColName "b"] == "a_b"
-- @
intercalateColNames :: String -> Array ColName -> String
intercalateColNames inter cs = Array.intercalate inter (escapeQuotes <<< unwrap <$> cs)

fromTableName :: TableName -> String
fromTableName (TableName tn) = "\"" <> escapeQuotes tn <> "\""

rawTableName :: TableName -> String
rawTableName (TableName tn) = escapeQuotes tn

mkColName :: String -> ColName
mkColName = ColName

mkTableName :: String -> TableName
mkTableName = TableName

-- | An inductively defined "tuple", or heterogeneous, non-empty list.
data NonEmptyHList a b = NEHL a b
infixr 1 type NonEmptyHList as :*:
infixr 1 NEHL as :*:
derive instance genericNonEmptyHList :: Generic (a :*: b) _
derive instance eqNonEmptyHList :: (Eq a, Eq b) => Eq (a :*: b)
derive instance ordNonEmptyHList :: (Ord a, Ord b) => Ord (a :*: b)
instance showNonEmptyHList :: (Show a, Show b) => Show (a :*: b) where
  show (a :*: b) = show a <> " :*: " <> show b

class Head i o | i -> o where
  tupHead :: i -> o
instance headNEHL :: Head (a :*: b) a where
  tupHead (a :*: b) = a
else
instance headSingle :: Head a a where
  tupHead a = a

first :: forall a o. Head a o => a -> o
first = tupHead

second :: forall a b o. Head b o => (a :*: b) -> o
second (_ :*: b) = tupHead b

third :: forall a b c o. Head c o => (a :*: b :*: c) -> o
third (_ :*: _ :*: c) = tupHead c

fourth :: forall a b c d o. Head d o => (a :*: b :*: c :*: d) -> o
fourth (_ :*: _ :*: _ :*: d) = tupHead d

fifth :: forall a b c d e o. Head e o => (a :*: b :*: c :*: d :*: e) -> o
fifth (_ :*: _ :*: _ :*: _ :*: e) = tupHead e
