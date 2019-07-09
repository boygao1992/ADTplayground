{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Orders.NoteAttribute where

import RIO
import Data.Aeson.TH.Util (deriveJSONDropTwo)
import Lens.Micro.TH.Util (makeLensesDropOne)

data NoteAttribute = NoteAttribute
  { __name :: !(Maybe Text)
    -- "name": "custom engraving",
  , __value :: !(Maybe Text)
    -- "value": "Happy Birthday"
  } deriving (Eq, Show)
$(makeLensesDropOne ''NoteAttribute)
$(deriveJSONDropTwo ''NoteAttribute)
