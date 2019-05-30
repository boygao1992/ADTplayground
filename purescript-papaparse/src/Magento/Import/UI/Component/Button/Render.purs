module Magento.Import.UI.Component.Button.Render where

import Effect.Aff.Class (class MonadAff)
import Halogen.HTML as HH
import Type.Data.Symbol (SProxy(..))
import Ocelot.Block.Button (button)
import Magento.Import.UI.Component.Button.Renderless (ComponentRender) as Button
import Magento.Import.UI.Component.Button.Setters (setProps) as Button

_button = SProxy :: SProxy "button"

simpleRender :: forall m. MonadAff m => Button.ComponentRender m
simpleRender _ =
  button (Button.setProps [])
  [ HH.text "+" ]
