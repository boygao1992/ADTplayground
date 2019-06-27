{-# LANGUAGE TemplateHaskell #-}
module Shopify.Api.Products.Req.GetProducts where

import RIO
import Lens.Micro.TH
import Data.Default
import Shopify.Data.Products.ProductId (ProductId(..))
import Shopify.Api.Products.Req.Data.PublishedStatus (PublishedStatus)

data Req = Req
  { _ids :: !(Maybe Text)
  , _limit :: !(Maybe Word8)
  , _since_id :: !(Maybe ProductId)
  , _title :: !(Maybe Text)
  , _vendor :: !(Maybe Text)
  , _handle :: !(Maybe Text)
  , _product_type :: !(Maybe Text)
  , _collection_id :: !(Maybe Text)
  , _created_at_min :: !(Maybe Text)
  , _created_at_max :: !(Maybe Text)
  , _updated_at_min :: !(Maybe Text)
  , _updated_at_max :: !(Maybe Text)
  , _published_at_min :: !(Maybe Text)
  , _published_at_max :: !(Maybe Text)
  , _published_status :: !(Maybe PublishedStatus)
  , _fields :: !(Maybe Text)
  , _presentment_currencies :: !(Maybe Text)
  } deriving (Eq, Show)
$(makeLenses ''Req)

instance Default Req where
  def = Req Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

