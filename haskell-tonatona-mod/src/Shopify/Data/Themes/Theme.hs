{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Themes.Theme where

import RIO
import Data.Aeson.TH.Util (deriveJSONDropTwo)
import Lens.Micro.TH.Util (makeLensesDropOne)
import Shopify.Data.Themes.ThemeId (ThemeId)
import Shopify.Data.Themes.Role (Role)

data Theme = Theme
  { __id :: !(Maybe ThemeId)
    -- "id": 73849602153,
  , __name :: !(Maybe Text)
    -- "name": "Debut",
  , __created_at :: !(Maybe Text)
    -- "created_at": "2019-06-07T10:37:05-04:00",
  , __updated_at :: !(Maybe Text)
    -- "updated_at": "2019-06-07T10:37:16-04:00",
  , __role :: !(Maybe Role)
    -- "role": "main",
  , __theme_store_id :: !(Maybe Word64)
    -- "theme_store_id": 796,
  , __previewable :: !(Maybe Bool)
    -- "previewable": true,
  , __processing :: !(Maybe Bool)
    -- "processing": false,
  , __admin_graphql_api_id :: !(Maybe Text)
    -- "admin_graphql_api_id": "gid://shopify/Theme/73849602153"
  } deriving (Eq, Show)
$(makeLensesDropOne ''Theme)
$(deriveJSONDropTwo ''Theme)

data SingleTheme = SingleTheme
  { __theme :: !Theme
  } deriving (Eq, Show)
$(makeLensesDropOne ''SingleTheme)
$(deriveJSONDropTwo ''SingleTheme)

data Themes = Themes
  { __themes :: ![Theme]
  } deriving (Eq, Show)
$(makeLensesDropOne ''Themes)
$(deriveJSONDropTwo ''Themes)
