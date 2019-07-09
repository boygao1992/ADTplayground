{-# LANGUAGE TemplateHaskell #-}
module Shopify.Api.Products.Req.GetProducts where

import RIO
import Lens.Micro.TH.Util
import Data.Default
import Shopify.Data.Products.ProductId (ProductId(..))
import Shopify.Api.Products.Req.Data.PublishedStatus (PublishedStatus)

data Req = Req
  { __ids :: !(Maybe Text)
  , __limit :: !(Maybe Word8)
  , __since_id :: !(Maybe ProductId)
  , __title :: !(Maybe Text)
  , __vendor :: !(Maybe Text)
  , __handle :: !(Maybe Text)
  , __product_type :: !(Maybe Text)
  , __collection_id :: !(Maybe Text)
  , __created_at_min :: !(Maybe Text)
  , __created_at_max :: !(Maybe Text)
  , __updated_at_min :: !(Maybe Text)
  , __updated_at_max :: !(Maybe Text)
  , __published_at_min :: !(Maybe Text)
  , __published_at_max :: !(Maybe Text)
  , __published_status :: !(Maybe PublishedStatus)
  , __fields :: !(Maybe Text)
  , __presentment_currencies :: !(Maybe Text)
  } deriving (Eq, Show)
$(makeLensesDropOne ''Req)

instance Default Req where
  def = Req Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

