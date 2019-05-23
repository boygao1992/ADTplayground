module Magento.Data.Categories where

import RIO
import RIO.List (intercalate)
import qualified RIO.Text as Text
import Data.ByteString.Char8 (split, unpack)
import Text.Read (readsPrec)

newtype Category = Category { unCategory :: [Text] }
  deriving newtype (Eq)
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
  deriving newtype (Eq, Show)
instance Read Categories where
  readsPrec _ input = [ (Categories . mapMaybe (readMaybe . unpack) . split ',' . fromString $ input, "")]

