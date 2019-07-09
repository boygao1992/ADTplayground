{-# LANGUAGE TemplateHaskell #-}
module Shopify.Api.Customers.Req.CreateAccountActivationUrl where

import RIO
import Lens.Micro.TH.Util
import Data.Aeson.TH.Util

data Res = Res
  { __account_activation_url :: !(Maybe Text)
  , __errors :: !(Maybe [Text])
  } deriving (Eq, Show)
$(makeLensesDropOne ''Res)
$(deriveJSONDropTwo ''Res)

