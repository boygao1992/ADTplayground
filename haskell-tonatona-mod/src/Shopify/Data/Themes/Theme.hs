{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Themes.Theme where

import RIO
import Data.Aeson.TH
import Shopify.Data.Themes.ThemeId (ThemeId)

data Theme = Theme
  { _id :: !(Maybe ThemeId)
    -- "id": 73849602153,
  , _name :: !(Maybe Text)
    -- "name": "Debut",
  , _created_at :: !(Maybe Text)
    -- "created_at": "2019-06-07T10:37:05-04:00",
  , _updated_at :: !(Maybe Text)
    -- "updated_at": "2019-06-07T10:37:16-04:00",
  , _role :: !(Maybe Text)
    -- "role": "main",
  , _theme_store_id :: !(Maybe Word64)
    -- "theme_store_id": 796,
  , _previewable :: !(Maybe Bool)
    -- "previewable": true,
  , _processing :: !(Maybe Bool)
    -- "processing": false,
  , _admin_graphql_api_id :: !(Maybe Text)
    -- "admin_graphql_api_id": "gid://shopify/Theme/73849602153"
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''Theme)

data SingleTheme = SingleTheme
  { _theme :: !Theme
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      }
    ''SingleTheme)

data Themes = Themes
  { _themes :: ![Theme]
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      }
    ''Themes)
