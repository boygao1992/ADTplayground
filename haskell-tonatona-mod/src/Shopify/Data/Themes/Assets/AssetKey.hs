module Shopify.Data.Themes.Assets.AssetKey where

import RIO
import Servant (FromHttpApiData, ToHttpApiData)
import Data.Aeson (FromJSON, ToJSON)

newtype AssetKey = AssetKey { unAssetKey :: Text }
  deriving newtype
    ( Eq, Ord, IsString
    , FromHttpApiData, ToHttpApiData
    , FromJSON, ToJSON
    )
  deriving (Show)
