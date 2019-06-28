{-# LANGUAGE TemplateHaskell #-}
module Shopify.Api.Customers.Req.CreateAccountActivationUrl where

import RIO
import Data.Aeson.TH

data Res = Res
  { _account_activation_url :: !(Maybe Text)
  , _errors :: !(Maybe [Text])
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''Res)

