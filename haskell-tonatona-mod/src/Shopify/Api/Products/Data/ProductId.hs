module Shopify.Api.Products.Data.ProductId where

import RIO
import Servant (FromHttpApiData, ToHttpApiData)
import Data.Aeson (FromJSON, ToJSON)

newtype ProductId = ProductId { unProductId :: Word64 }
  deriving newtype
    ( Eq, Ord
    , FromHttpApiData, ToHttpApiData
    , FromJSON, ToJSON
    )
  deriving (Show)
