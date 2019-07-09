{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Orders.Transactions.PaymentDetail where

import RIO
import Data.Aeson.TH.Util (deriveJSONDropTwo)
import Lens.Micro.TH.Util (makeLensesDropOne)

data PaymentDetail = PaymentDetail
  { __avs_result_code :: !(Maybe Text)
    -- "avs_result_code": "123456",
  , __credit_card_bin :: !(Maybe Text)
    -- "credit_card_bin": "M",
  , __cvv_result_code :: !(Maybe Text)
    -- "cvv_result_code": "M",
  , __credit_card_number :: !(Maybe Text)
    -- "credit_card_number": "•••• •••• •••• 4242",
  , __credit_card_company :: !(Maybe Text)
    -- "credit_card_company": "Visa"
  } deriving (Eq, Show)
$(makeLensesDropOne ''PaymentDetail)
$(deriveJSONDropTwo ''PaymentDetail)
