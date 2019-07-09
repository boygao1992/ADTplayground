{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Orders.ClientDetails where

import RIO
import Data.Aeson.TH.Util (deriveJSONDropTwo)
import Lens.Micro.TH.Util (makeLensesDropOne)

data ClientDetails = ClientDetails
  { __accept_language :: !(Maybe Text)
  -- "accept_language": "en-US,en;q=0.9",
  , __browser_height :: !(Maybe Word32)
  -- "browser_height": 1320,
  , __browser_ip :: !(Maybe Text)
  -- "browser_ip": "216.191.105.146",
  , __browser_width :: !(Maybe Word32)
  -- "browser_width": 1280,
  , __session_hash :: !(Maybe Text)
  -- "session_hash": "9ad4d1f4e6a8977b9dd98eed1e477643",
  , __user_agent :: !(Maybe Text)
  -- "user_agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/62.0.3202.94 Safari/537.36"
  } deriving (Eq, Show)
$(makeLensesDropOne ''ClientDetails)
$(deriveJSONDropTwo ''ClientDetails)
