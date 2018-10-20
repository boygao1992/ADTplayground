module Main where

import Prelude

import Data.Maybe (fromMaybe)
import AutoSize as AutoSize
import Effect (Effect)
import Web.DOM.ParentNode (QuerySelector(..))
import Halogen.VDom.Driver (runUI)
import Halogen.Aff (runHalogenAff, awaitBody, selectElement) as HA

main :: Effect Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    app <- HA.selectElement (QuerySelector "#app")
    runUI
      ( AutoSize.component
      )
      unit -- Input
      (fromMaybe body app) -- HTMLElement
