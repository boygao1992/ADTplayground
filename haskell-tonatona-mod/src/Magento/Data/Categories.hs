module Magento.Data.Categories where

import RIO hiding (Category)
import qualified RIO.List as List (intercalate, lastMaybe, length)
import qualified RIO.Text as Text
import Data.Aeson (FromJSON, ToJSON, Value(String), parseJSON, toEncoding)
import Data.ByteString.Char8 (split, unpack)
import Text.Read (readsPrec)
import Control.Newtype.Generics (Newtype)

allLeafNodes :: Categories -> [Text]
allLeafNodes (Categories cs) = mapMaybe leafNode cs

leafNode :: Category -> Maybe Text
leafNode (Category c) = List.lastMaybe c

length :: Category -> Int
length = List.length . unCategory

newtype Category = Category { unCategory :: [Text] }
  deriving newtype (Eq, Semigroup, Monoid)
  deriving (Generic)
instance Newtype Category
instance Read Category where
  readsPrec _
    = either (const []) id
    . fmap (\x -> [(Category . fmap Text.strip $ x, "")])
    . traverse decodeUtf8'
    . split '/'
    . fromString
instance Show Category where
  show = List.intercalate "/" . fmap Text.unpack . unCategory
instance FromJSON Category where
  parseJSON (String cText) = case readMaybe $ Text.unpack cText of
    Nothing -> fail "invalid Category encoding"
    Just c -> pure $ c
  parseJSON _ = fail "invalid Category encoding"
instance ToJSON Category where
  toEncoding = toEncoding . show

newtype Categories = Categories { unCategories :: [Category] }
  deriving newtype (Eq, Semigroup, Monoid)
  deriving (Generic)
instance Newtype Categories
instance Read Categories where
  readsPrec _ input = [ (Categories . mapMaybe (readMaybe . unpack) . split ',' . fromString $ input, "")]
instance Show Categories where
  show = List.intercalate "," . fmap show . unCategories
instance FromJSON Categories where
  parseJSON (String csText) = case readMaybe $ Text.unpack csText of
    Nothing -> fail "invalid Categories encoding"
    Just cs -> pure $ cs
  parseJSON _ = fail "invalid Categories encoding"
instance ToJSON Categories where
  toEncoding = toEncoding . show
