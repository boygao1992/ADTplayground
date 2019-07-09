{-# LANGUAGE TemplateHaskell #-}
module Data.Aeson.TH.Util where

import RIO
import Data.Aeson.TH
import Language.Haskell.TH

deriveJSONDropTwo :: Name -> Q [Dec]
deriveJSONDropTwo = deriveJSON
  defaultOptions
    { fieldLabelModifier = drop 2
    , omitNothingFields = True
    }
