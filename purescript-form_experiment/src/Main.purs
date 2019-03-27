module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen.Form as Form
import Halogen.HTML as HH
import Web.HTML.HTMLElement (HTMLElement)

run body =
  runUI
  Form.component
  (Form.text $ Just "first input two three")
  body

rerunUI :: HTMLElement -> Effect Unit
rerunUI body = HA.runHalogenAff $ run body

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  run body
