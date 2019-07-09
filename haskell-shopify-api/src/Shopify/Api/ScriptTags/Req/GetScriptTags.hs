{-# LANGUAGE TemplateHaskell #-}
module Shopify.Api.ScriptTags.Req.GetScriptTags where

import RIO
import Data.Default
import Lens.Micro.TH

import Shopify.Data.ScriptTags.ScriptTagId (ScriptTagId)

data Req = Req
  { _limit :: Maybe Word8
  , _since_id :: Maybe ScriptTagId
  , _created_at_min :: Maybe Text
  , _created_at_max :: Maybe Text
  , _updated_at_min :: Maybe Text
  , _updated_at_max :: Maybe Text
  , _src :: Maybe Text
  , _fields :: Maybe Text
  }
$(makeLenses ''Req)

instance Default Req where
  def = Req Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
