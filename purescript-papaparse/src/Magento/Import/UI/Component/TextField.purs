module Magento.Import.UI.Component.TextField
( module TextField
, _textField
, render
) where

import Effect.Aff.Class (class MonadAff)
import Magento.Import.UI.Component.Renderless.TextField (Action(..), ChildSlots, Component, ComponentHTML, ComponentM, ComponentRender, Input, Output(..), Query, Slot, State, StateStore, component, defaultInitialState, handleAction) as TextField
import Magento.Import.UI.Component.Renderless.TextField.Setters (Props, setProps) as TextField
import Ocelot.Block.Input (input)
import Type.Data.Symbol (SProxy(..))

_textField = SProxy :: SProxy "textField"

render :: forall m. MonadAff m => TextField.ComponentRender m
render st =
  input (TextField.setProps st [])
