module Main where

import Prelude
import Data.Maybe (fromMaybe)
import Effect (Effect)
-- import Effect.Console (log)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Search as Search
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  app <- HA.selectElement (QuerySelector "#app")
  runUI
    Search.component
    unit
    (fromMaybe body app)
