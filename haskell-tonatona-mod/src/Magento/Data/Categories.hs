module Magento.Data.Categories where

import RIO
import Data.ByteString.Char8 (split, unpack)
import Text.Read (readsPrec)

newtype Category = Category { unCategory :: [Text] }
  deriving newtype (Eq, Show)
instance Read Category where
  readsPrec _
    = either (const []) id
    . fmap (\x -> [(Category x, "")])
    . traverse decodeUtf8'
    . split '/'
    . fromString
newtype Categories = Categories { unCategories :: [Category] }
  deriving newtype (Eq, Show)
instance Read Categories where
  readsPrec _ input = [ (Categories . mapMaybe (readMaybe . unpack) . split ',' . fromString $ input, "")]

