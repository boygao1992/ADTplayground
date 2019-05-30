module Magento.Data.Categories where

import RIO
import RIO.List (intercalate, lastMaybe)
import qualified RIO.Text as Text
import Data.ByteString.Char8 (split, unpack)
import Text.Read (readsPrec)
import Control.Newtype.Generics (Newtype)

allLeafNodes :: Categories -> [Text]
allLeafNodes (Categories cs) = mapMaybe leafNode cs

leafNode :: Category -> Maybe Text
leafNode (Category c) = lastMaybe c

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
  show = intercalate "/" . fmap Text.unpack . unCategory

newtype Categories = Categories { unCategories :: [Category] }
  deriving newtype (Eq, Semigroup, Monoid)
  deriving (Generic)
instance Newtype Categories
instance Read Categories where
  readsPrec _ input = [ (Categories . mapMaybe (readMaybe . unpack) . split ',' . fromString $ input, "")]
instance Show Categories where
  show = intercalate "," . fmap show . unCategories
