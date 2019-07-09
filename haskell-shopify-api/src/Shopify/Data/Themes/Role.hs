{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Themes.Role where

import RIO
import Data.Aeson (FromJSON, ToJSON, Value(String), parseJSON, toJSON)

data Role
  = Main
  | Unpublished
  | Demo
  deriving (Eq, Ord, Show, Enum)

instance ToJSON Role where
  toJSON Main        = "main"
  toJSON Unpublished = "unpublished"
  toJSON Demo        = "demo"

instance FromJSON Role where
  parseJSON (String "main") = pure Main
  parseJSON (String "unpublished") = pure Unpublished
  parseJSON (String "demo") = pure Demo
  parseJSON _ = fail "Unrecognized theme role"
