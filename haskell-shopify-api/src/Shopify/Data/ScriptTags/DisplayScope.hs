{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.ScriptTags.DisplayScope where

import RIO
import Data.Aeson (FromJSON, ToJSON, Value(String), parseJSON, toJSON)

data DisplayScope
  = OnlineStore
  | OrderStatus
  | All
  deriving (Eq, Ord, Show, Enum)

instance ToJSON DisplayScope where
  toJSON OnlineStore = "online_store"
  toJSON OrderStatus = "order_status"
  toJSON All         = "all"

instance FromJSON DisplayScope where
  parseJSON (String "online_store") = pure OnlineStore
  parseJSON (String "order_status") = pure OrderStatus
  parseJSON (String "all") = pure All
  parseJSON _ = fail "Unrecognized ScriptTag DisplayScope"
