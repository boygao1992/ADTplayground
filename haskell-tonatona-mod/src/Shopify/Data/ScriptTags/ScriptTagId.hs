module Shopify.Data.ScriptTags.ScriptTagId where

import RIO
import Servant (FromHttpApiData, ToHttpApiData)
import Data.Aeson (FromJSON, ToJSON)

newtype ScriptTagId = ScriptTagId { unScriptTagId :: Word64 }
  deriving newtype
    ( Eq, Ord
    , FromHttpApiData, ToHttpApiData
    , FromJSON, ToJSON
    )
  deriving (Show)
