module Main where

import Prelude

import Effect (Effect)
import Magento.Import.UI.Page (component)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body
