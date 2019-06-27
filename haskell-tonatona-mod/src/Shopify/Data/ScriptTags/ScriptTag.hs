{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.ScriptTags.ScriptTag where

import RIO
import Data.Aeson.TH
import Shopify.Data.ScriptTags.ScriptTagId (ScriptTagId)
import Shopify.Data.ScriptTags.Event (Event)
import Shopify.Data.ScriptTags.DisplayScope (DisplayScope)

data ScriptTag = ScriptTag
  { _id :: !(Maybe ScriptTagId)
    -- "id": 596726825,
  , _src :: !(Maybe Text)
    -- "src": "https://js-aplenty.com/foo.js",
  , _event :: !(Maybe Event)
    -- "event": "onload",
  , _created_at :: !(Maybe Text)
    -- "created_at": "2019-04-16T16:37:57-04:00",
  , _updated_at :: !(Maybe Text)
    -- "updated_at": "2019-04-16T16:37:57-04:00",
  , _display_scope :: !(Maybe DisplayScope)
    -- DONE "online_store" | "order_status" | "all"
    -- "display_scope": "all"
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''ScriptTag)

data SingleScriptTag = SingleScriptTag
  { _script_tag :: !ScriptTag
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      }
    ''SingleScriptTag)

data ScriptTags = ScriptTags
  { _script_tags :: ![ScriptTag]
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      }
    ''ScriptTags)
