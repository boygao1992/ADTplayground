{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.Themes.Assets.Asset where

import RIO
import Data.Aeson.TH.Util (deriveJSONDropTwo)
import Lens.Micro.TH.Util (makeLensesDropOne)
import Shopify.Data.Themes.ThemeId (ThemeId)
import Shopify.Data.Themes.Assets.AssetKey (AssetKey)

data Asset = Asset
  { __key :: !(Maybe AssetKey)
    -- "key": "templates/customers/account.liquid",
    -- "key": "assets/ajax-loader.gif",
  , __public_url :: !(Maybe Text) -- TODO
    -- "public_url": "http://static.shopify.com/assets/bg.gif?1",
  , __created_at :: !(Maybe Text)
    -- "created_at": "2019-06-07T10:37:07-04:00",
  , __updated_at :: !(Maybe Text)
    -- "updated_at": "2019-06-07T10:37:07-04:00",
  , __size :: !(Maybe Word64)
    -- "size": 2198,
  , __theme_id :: !(Maybe ThemeId)
    -- "theme_id": 73849602153

  , __content_type :: !(Maybe Text)
    -- "content_type": "text/x-liquid",
    -- "content_type": "image/gif",
  , __value :: !(Maybe Text) -- NOTE Liquid markup of a template file.
    -- "value": "<div id=\"page\">\n<h1>404 Page not found</h1>\n<p>We couldn't find the page you were looking for.</p>\n</div>"
  , __attachment :: !(Maybe Text) -- NOTE A base64-encoded image.
    -- "attachment": "R0lGODlhAQABAPABAP///wAAACH5Ow==\n"

  -- NOTE PUT /admin/api/2019-04/themes/#{theme_id}/assets.json
  , __src :: !(Maybe Text)
  -- The source URL of an image. Include in the body of the PUT request to upload the image to Shopify.
  , __source_key :: !(Maybe AssetKey)
  -- The path within the theme to an existing asset. Include in the body of the PUT request to create a duplicate asset.
  } deriving (Eq, Show)
$(makeLensesDropOne ''Asset)
$(deriveJSONDropTwo ''Asset)

data SingleAsset = SingleAsset
  { __asset :: !Asset
  } deriving (Eq, Show)
$(makeLensesDropOne ''SingleAsset)
$(deriveJSONDropTwo ''SingleAsset)

data Assets = Assets
  { __assets :: ![Asset]
  } deriving (Eq, Show)
$(makeLensesDropOne ''Assets)
$(deriveJSONDropTwo ''Assets)
