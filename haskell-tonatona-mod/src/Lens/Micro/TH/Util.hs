{-# LANGUAGE TemplateHaskell #-}
module Lens.Micro.TH.Util where

import RIO
import Language.Haskell.TH
import Lens.Micro ((.~))
import Lens.Micro.TH

makeLensesDropOne :: Name -> DecsQ
makeLensesDropOne = makeLensesWith
  $ lensRules
  & lensField .~ \_ _ label -> [TopName $ mkName $ drop 1 $ nameBase $ label]
