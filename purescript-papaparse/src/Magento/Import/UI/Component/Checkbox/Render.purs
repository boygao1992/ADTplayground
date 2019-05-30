module Magento.Import.UI.Component.Checkbox.Render where

import Effect.Aff.Class (class MonadAff)
import Magento.Import.UI.Component.Checkbox.Renderless (ComponentRender) as Checkbox
import Magento.Import.UI.Component.Checkbox.Setters (setProps) as Checkbox
import Ocelot.Block.Checkbox (checkbox_)
import Type.Data.Symbol (SProxy(..))

_checkbox = SProxy :: SProxy "checkbox"

simpleRender :: forall m. MonadAff m => Checkbox.ComponentRender m
simpleRender st =
  checkbox_ (Checkbox.setProps st [])
  []

