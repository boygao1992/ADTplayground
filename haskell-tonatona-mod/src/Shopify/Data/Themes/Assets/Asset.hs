{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Themes.Assets.Asset where

import RIO
import Data.Aeson.TH
import Shopify.Data.Themes.ThemeId (ThemeId)
import Shopify.Data.Themes.Assets.AssetKey (AssetKey)

data Asset = Asset
  { _key :: !(Maybe AssetKey)
    -- "key": "templates/customers/account.liquid",
    -- "key": "assets/ajax-loader.gif",
  , _public_url :: !(Maybe Text)
    -- "public_url": "http://static.shopify.com/assets/bg.gif?1",
  , _created_at :: !(Maybe Text)
    -- "created_at": "2019-06-07T10:37:07-04:00",
  , _updated_at :: !(Maybe Text)
    -- "updated_at": "2019-06-07T10:37:07-04:00",
  , _size :: !(Maybe Word64)
    -- "size": 2198,
  , _theme_id :: !(Maybe ThemeId)
    -- "theme_id": 73849602153

  , _content_type :: !(Maybe Text)
    -- "content_type": "text/x-liquid",
    -- "content_type": "image/gif",
  , _value :: !(Maybe String) -- NOTE Liquid markup of a template file.
    -- "value": "<div id=\"page\">\n<h1>404 Page not found</h1>\n<p>We couldn't find the page you were looking for.</p>\n</div>"
  , _attachment :: !(Maybe String) -- NOTE A base64-encoded image.
    -- "attachment": "R0lGODlhAQABAPABAP///wAAACH5Ow==\n"
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''Asset)

data SingleAsset = SingleAsset
  { _asset :: !Asset
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      }
    ''SingleAsset)

data Assets = Assets
  { _assets :: ![Asset]
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      }
    ''Assets)
