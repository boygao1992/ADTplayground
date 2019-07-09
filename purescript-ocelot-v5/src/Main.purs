module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Page (component)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI) as HD
import Web.HTML (HTMLElement)

runUI :: HTMLElement -> Aff Unit
runUI = void <<< HD.runUI component unit

reRunUI :: HTMLElement -> Effect Unit
reRunUI body = HA.runHalogenAff $ runUI body

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI body
