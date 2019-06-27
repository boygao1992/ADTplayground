{-# LANGUAGE TemplateHaskell #-}
module Shopify.Api.Customers.Addresses.Req.GetAddresses where

import RIO
import Lens.Micro.TH
import Data.Default

data Req = Req
  { _limit :: !(Maybe Word64)
  , _page :: !(Maybe Word64)
  } deriving (Eq, Show)
$(makeLenses ''Req)

instance Default Req where
  def = Req Nothing Nothing
