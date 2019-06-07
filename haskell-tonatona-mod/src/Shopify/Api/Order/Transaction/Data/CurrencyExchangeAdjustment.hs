{-# LANGUAGE TemplateHaskell #-}
module Shopify.Api.Order.Transaction.Data.CurrencyExchangeAdjustment where

import RIO
import Data.Aeson.TH

data CurrencyExchangeAdjustment = CurrencyExchangeAdjustment
  { _id :: !(Maybe Word32)
    -- "id": 1,
  , _adjustment :: !(Maybe Text)
    -- "adjustment": "-0.01",
  , _original_amount :: !(Maybe Text)
    -- "original_amount": "-53.62",
  , _final_amount :: !(Maybe Text)
    -- "final_amount": "-53.63",
  , _currency :: !(Maybe Text)
    -- "currency": "CAD"
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''CurrencyExchangeAdjustment)
