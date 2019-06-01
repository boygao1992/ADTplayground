module Magento.Import.Data.Skus where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.String as String
import Data.String.Read (class Read)

newtype Sku = Sku String
derive newtype instance eqSku :: Eq Sku
derive newtype instance ordSku :: Ord Sku
derive newtype instance readSku :: Read Sku
derive newtype instance semigroupSku :: Semigroup Sku
derive newtype instance monoidSku :: Monoid Sku
derive instance newtypeSku :: Newtype Sku _
instance showSku :: Show Sku where
  show = unwrap

newtype Skus = Skus (Array Sku)

derive newtype instance eqSkus :: Eq Skus
derive newtype instance semigroupSkus :: Semigroup Skus
derive newtype instance monoidSkus :: Monoid Skus
derive instance newtypeSkus :: Newtype Skus _

instance readSkus :: Read Skus where
  read = Just <<< Skus <<< map (Sku <<< String.trim) <<< String.split (String.Pattern ",")
instance showSkus :: Show Skus where
  show = Array.intercalate "," <<< map show <<< unwrap

newtype RelatedSkus = RelatedSkus (Skus)
derive newtype instance eqRelatedSkus :: Eq RelatedSkus
instance newtypeRelatedSkus :: Newtype RelatedSkus (Array Sku) where
  unwrap (RelatedSkus (Skus skus)) = skus
  wrap skus = RelatedSkus $ Skus skus
derive newtype instance readRelatedSkus :: Read RelatedSkus
derive newtype instance showRelatedSkus :: Show RelatedSkus

newtype CrossSellSkus = CrossSellSkus (Skus)
derive newtype instance eqCrossSellSkus :: Eq CrossSellSkus
instance newtypeCrossSellSkus :: Newtype CrossSellSkus (Array Sku) where
  unwrap (CrossSellSkus (Skus skus)) = skus
  wrap skus = CrossSellSkus $ Skus skus
derive newtype instance readCrossSellSkus :: Read CrossSellSkus
derive newtype instance showCrossSellSkus :: Show CrossSellSkus

newtype UpSellSkus = UpSellSkus (Skus)
derive newtype instance eqUpSellSkus :: Eq UpSellSkus
instance newtypeUpSellSkus :: Newtype UpSellSkus (Array Sku) where
  unwrap (UpSellSkus (Skus skus)) = skus
  wrap skus = UpSellSkus $ Skus skus
derive newtype instance readUpSellSkus :: Read UpSellSkus
derive newtype instance showUpSellSkus :: Show UpSellSkus
