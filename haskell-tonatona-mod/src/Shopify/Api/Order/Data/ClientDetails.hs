{-# LANGUAGE TemplateHaskell #-}
module Shopify.Api.Order.Data.ClientDetails where

import RIO
import Data.Aeson.TH

data ClientDetails = ClientDetails
  { _accept_language :: !(Maybe Text)
  -- "accept_language": "en-US,en;q=0.9",
  , _browser_height :: !(Maybe Word32)
  -- "browser_height": 1320,
  , _browser_ip :: !(Maybe Text)
  -- "browser_ip": "216.191.105.146",
  , _browser_width :: !(Maybe Word32)
  -- "browser_width": 1280,
  , _session_hash :: !(Maybe Text)
  -- "session_hash": "9ad4d1f4e6a8977b9dd98eed1e477643",
  , _user_agent :: !(Maybe Text)
  -- "user_agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/62.0.3202.94 Safari/537.36"
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''ClientDetails)
