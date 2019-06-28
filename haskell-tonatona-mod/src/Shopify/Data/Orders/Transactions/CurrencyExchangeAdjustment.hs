{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Orders.Transactions.CurrencyExchangeAdjustment where

import RIO
import Data.Aeson.TH.Util (deriveJSONDropTwo)
import Lens.Micro.TH.Util (makeLensesDropOne)

data CurrencyExchangeAdjustment = CurrencyExchangeAdjustment
  { __id :: !(Maybe Word64)
    -- "id": 1,
  , __adjustment :: !(Maybe Text)
    -- "adjustment": "-0.01",
  , __original_amount :: !(Maybe Text)
    -- "original_amount": "-53.62",
  , __final_amount :: !(Maybe Text)
    -- "final_amount": "-53.63",
  , __currency :: !(Maybe Text)
    -- "currency": "CAD"
  } deriving (Eq, Show)
$(makeLensesDropOne ''CurrencyExchangeAdjustment)
$(deriveJSONDropTwo ''CurrencyExchangeAdjustment)

