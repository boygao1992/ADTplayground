{-# LANGUAGE TemplateHaskell #-}
module Shopify.Api.Customers.Req.SendAccountInvite where

import RIO
import Data.Aeson.TH

data CustomerInvite = CustomerInvite
  { _to :: !(Maybe Text)
  , _from :: !(Maybe Text)
  , _subject :: !(Maybe Text)
  , _custom_message :: !(Maybe Text)
  , _bcc :: !(Maybe [Text])
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''CustomerInvite)

data SingleCustomerInvite = SingleCustomerInvite
  { _customer_invite :: !CustomerInvite
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''SingleCustomerInvite)

