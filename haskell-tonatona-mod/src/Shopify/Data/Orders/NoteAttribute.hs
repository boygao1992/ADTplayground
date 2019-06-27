{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Orders.NoteAttribute where

import RIO
import Data.Aeson.TH

data NoteAttribute = NoteAttribute
  { _name :: !(Maybe Text)
    -- "name": "custom engraving",
  , _value :: !(Maybe Text)
    -- "value": "Happy Birthday"
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''NoteAttribute)
