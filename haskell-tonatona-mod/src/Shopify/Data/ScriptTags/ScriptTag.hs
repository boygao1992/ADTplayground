{-# LANGUAGE TemplateHaskell #-}
module Shopify.Data.ScriptTags.ScriptTag where

import RIO
import Data.Aeson.TH.Util (deriveJSONDropTwo)
import Lens.Micro.TH.Util (makeLensesDropOne)

import Shopify.Data.ScriptTags.ScriptTagId (ScriptTagId)
import Shopify.Data.ScriptTags.Event (Event)
import Shopify.Data.ScriptTags.DisplayScope (DisplayScope)

data ScriptTag = ScriptTag
  { __id :: !(Maybe ScriptTagId)
    -- "id": 596726825,
  , __src :: !(Maybe Text)
    -- "src": "https://js-aplenty.com/foo.js",
  , __event :: !(Maybe Event)
    -- "event": "onload",
  , __created_at :: !(Maybe Text)
    -- "created_at": "2019-04-16T16:37:57-04:00",
  , __updated_at :: !(Maybe Text)
    -- "updated_at": "2019-04-16T16:37:57-04:00",
  , __display_scope :: !(Maybe DisplayScope)
    -- DONE "online_store" | "order_status" | "all"
    -- "display_scope": "all"
  } deriving (Eq, Show)
$(makeLensesDropOne ''ScriptTag)
$(deriveJSONDropTwo ''ScriptTag)

data SingleScriptTag = SingleScriptTag
  { __script_tag :: !ScriptTag
  } deriving (Eq, Show)
$(deriveJSONDropTwo ''SingleScriptTag)

data ScriptTags = ScriptTags
  { __script_tags :: ![ScriptTag]
  } deriving (Eq, Show)
$(deriveJSONDropTwo ''ScriptTags)
