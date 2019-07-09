{-# LANGUAGE TemplateHaskell #-}
module Shopify.Api.Customers.Req.SendAccountInvite where

import RIO
import Lens.Micro.TH.Util
import Data.Aeson.TH.Util

data CustomerInvite = CustomerInvite
  { __to :: !(Maybe Text)
  , __from :: !(Maybe Text)
  , __subject :: !(Maybe Text)
  , __custom_message :: !(Maybe Text)
  , __bcc :: !(Maybe [Text])
  } deriving (Eq, Show)
$(makeLensesDropOne ''CustomerInvite)
$(deriveJSONDropTwo ''CustomerInvite)

data SingleCustomerInvite = SingleCustomerInvite
  { __customer_invite :: !CustomerInvite
  } deriving (Eq, Show)
$(makeLensesDropOne ''SingleCustomerInvite)
$(deriveJSONDropTwo ''SingleCustomerInvite)

