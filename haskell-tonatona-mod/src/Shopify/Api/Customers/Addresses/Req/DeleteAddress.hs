{-# LANGUAGE TemplateHaskell #-}
module Shopify.Api.Customers.Addresses.Req.DeleteAddress where

import RIO
import Data.Aeson.TH

data Base = Base
  { _base :: ![Text]
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      }
    ''Base)

data Res = Res
  { _errors :: !(Maybe Base)
  }
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''Res)
