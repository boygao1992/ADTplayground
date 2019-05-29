module Magento.Import.UI.Component.Checkbox
( module Checkbox
, _checkbox
, render
) where

import Effect.Aff.Class (class MonadAff)
import Magento.Import.UI.Component.Renderless.Checkbox (Action(..), ChildSlots, Component, ComponentHTML, ComponentM, ComponentRender, Input, Output(..), Query, Slot, State, StateStore, component, defaultInitialState, handleAction) as Checkbox
import Magento.Import.UI.Component.Renderless.Checkbox.Setters (Props, setProps) as Checkbox
import Ocelot.Block.Checkbox (checkbox_)
import Type.Data.Symbol (SProxy(..))

_checkbox = SProxy :: SProxy "checkbox"

render :: forall m. MonadAff m => Checkbox.ComponentRender m
render st =
  checkbox_ (Checkbox.setProps st [])
  []

