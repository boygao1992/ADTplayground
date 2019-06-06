{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Response where

import RIO
import Data.Aeson.TH

data Base = Base
  { _base :: ![Text]
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      }
    ''Base)

data Errors = Errors
  { _errors :: !(Maybe Base)
  }
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''Errors)
