module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
-- import Effect.Console (log)
-- import Magento.Import.UI.Page (component)
-- import Polaris.UI.Page (component)
-- import Polaris.UI.Component.Table.Cell (component)
-- import Polaris.UI.Page2 (component)
import Polaris.UI.PageSvg (component)
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

-- main :: String -> Effect Unit
-- main hostname = do
--   -- TODO debug
--   log "main"

--   HA.runHalogenAff do
--     body <- HA.awaitBody
--     void $ HD.runUI component hostname body
