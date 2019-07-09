{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.ScriptTags.Event where

import RIO
import Data.Aeson (FromJSON, ToJSON, Value(String), parseJSON, toJSON)

data Event
  = Onload
  deriving (Eq, Ord, Show, Enum)

instance ToJSON Event where
  toJSON Onload = "onload"

instance FromJSON Event where
  parseJSON (String "onload") = pure Onload
  parseJSON _ = fail "Unrecognized ScriptTag Event"
