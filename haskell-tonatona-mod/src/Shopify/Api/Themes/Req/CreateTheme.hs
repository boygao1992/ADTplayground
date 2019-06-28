{-# LANGUAGE TemplateHaskell #-}
module Shopify.Api.Themes.Req.CreateTheme where

import RIO
import Data.Aeson.TH

import Shopify.Data.Themes.Role (Role)

data Payload = Payload
  { _name :: !Text
  , _src :: !Text
  , _role :: !Role
  }
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''Payload)

data Req = Req
  { _theme :: !Payload
  }
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      }
    ''Req)
