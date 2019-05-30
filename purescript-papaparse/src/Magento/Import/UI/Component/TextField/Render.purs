module Magento.Import.UI.Component.TextField.Render where

import Effect.Aff.Class (class MonadAff)
import Magento.Import.UI.Component.TextField.Renderless (ComponentRender) as TextField
import Magento.Import.UI.Component.TextField.Setters (setProps) as TextField
import Ocelot.Block.Input (input)
import Type.Data.Symbol (SProxy(..))

_textField = SProxy :: SProxy "textField"

simpleRender :: forall m. MonadAff m => TextField.ComponentRender m
simpleRender st =
  input (TextField.setProps st [])
