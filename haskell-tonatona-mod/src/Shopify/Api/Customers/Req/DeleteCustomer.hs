{-# LANGUAGE TemplateHaskell #-}
module Shopify.Api.Customers.Req.DeleteCustomer where

import RIO
import Data.Aeson.TH

data Res = Res
  { _errors :: !(Maybe Text)
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''Res)
