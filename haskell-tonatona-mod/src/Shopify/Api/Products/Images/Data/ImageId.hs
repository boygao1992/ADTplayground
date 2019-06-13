module Shopify.Api.Products.Images.Data.ImageId where

import RIO
import Servant (FromHttpApiData, ToHttpApiData)
import Data.Aeson (FromJSON, ToJSON)

newtype ImageId = ImageId { unImageId :: Word64 }
  deriving newtype
    ( Eq, Ord
    , FromHttpApiData, ToHttpApiData
    , FromJSON, ToJSON
    )
  deriving (Show)
