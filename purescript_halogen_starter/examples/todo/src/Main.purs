module Examples.Todo.Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import List as List

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI List.component unit body
