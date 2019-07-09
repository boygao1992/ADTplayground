{-# LANGUAGE TemplateHaskell #-}
module Shopify.Api.Products.Req.CountProducts where

import RIO
import Lens.Micro.TH.Util
import Data.Aeson.TH.Util
import Data.Default
import Shopify.Api.Products.Req.Data.PublishedStatus (PublishedStatus)

data Req = Req
  { __vendor :: !(Maybe Text)
  , __product_type :: !(Maybe Text)
  , __collection_id :: !(Maybe Text)
  , __created_at_min :: !(Maybe Text)
  , __created_at_max :: !(Maybe Text)
  , __updated_at_min :: !(Maybe Text)
  , __updated_at_max :: !(Maybe Text)
  , __published_at_min :: !(Maybe Text)
  , __published_at_max :: !(Maybe Text)
  , __published_status :: !(Maybe PublishedStatus)
  } deriving (Eq, Show)
$(makeLensesDropOne ''Req)

instance Default Req where
  def = Req Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data Res = Res
  { __count :: !(Maybe Word64)
  } deriving (Eq, Show)
$(makeLensesDropOne ''Res)
$(deriveJSONDropTwo ''Res)

