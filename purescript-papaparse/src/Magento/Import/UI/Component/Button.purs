module Magento.Import.UI.Component.Button
( module Button
, _button
, render
) where

import Effect.Aff.Class (class MonadAff)
import Halogen.HTML as HH
import Type.Data.Symbol (SProxy(..))
import Ocelot.Block.Button (button)
import Magento.Import.UI.Component.Renderless.Button (Action(..), ChildSlots, Component, ComponentHTML, ComponentM, ComponentRender, Input, Output(..), Query, Slot, State, StateStore, component, handleAction, initialState) as Button
import Magento.Import.UI.Component.Renderless.Button.Setters (Props, setProps) as Button

_button = SProxy :: SProxy "button"

render :: forall m. MonadAff m => Button.ComponentRender m
render _ =
  button (Button.setProps [])
  [ HH.text "+" ]
