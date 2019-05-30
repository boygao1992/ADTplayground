module Magento.Data.Skus where

import RIO
import RIO.List (intercalate)
import qualified RIO.Text as Text
import Data.ByteString.Char8 (split)
import Text.Read (readsPrec)

newtype Sku = Sku { unSku :: Text }
  deriving newtype (Eq, Ord, Read, Semigroup, Monoid)
instance Show Sku where
  show = Text.unpack . unSku

newtype Skus = Skus { unSkus :: [Sku] }
  deriving newtype (Eq, Semigroup, Monoid)
instance Read Skus where
  readsPrec _
    = either (const []) id
    . fmap (\x -> [(Skus . fmap (Sku . Text.strip) $ x, "")])
    . traverse decodeUtf8'
    . split ','
    . fromString
instance Show Skus where
  show = intercalate "," . fmap show . unSkus
