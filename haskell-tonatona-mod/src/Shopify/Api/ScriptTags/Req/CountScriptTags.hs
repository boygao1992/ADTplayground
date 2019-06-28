{-# LANGUAGE TemplateHaskell #-}
module Shopify.Api.ScriptTags.Req.CountScriptTags where

import RIO
import Data.Aeson.TH

data Res = Res
  { _count :: !(Maybe Word64)
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''Res)

