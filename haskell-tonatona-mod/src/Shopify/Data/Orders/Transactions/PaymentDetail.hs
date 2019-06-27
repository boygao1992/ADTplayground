{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Orders.Transactions.PaymentDetail where

import RIO
import Data.Aeson.TH

data PaymentDetail = PaymentDetail
  { _avs_result_code :: !(Maybe Text)
    -- "avs_result_code": "123456",
  , _credit_card_bin :: !(Maybe Text)
    -- "credit_card_bin": "M",
  , _cvv_result_code :: !(Maybe Text)
    -- "cvv_result_code": "M",
  , _credit_card_number :: !(Maybe Text)
    -- "credit_card_number": "•••• •••• •••• 4242",
  , _credit_card_company :: !(Maybe Text)
    -- "credit_card_company": "Visa"
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''PaymentDetail)
