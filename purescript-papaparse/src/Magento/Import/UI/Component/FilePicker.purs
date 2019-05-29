module Magento.Import.UI.Component.FilePicker
( module FilePicker
, _filePicker
, render
) where

import Effect.Aff.Class (class MonadAff)
import Halogen.HTML as HH
import Magento.Import.UI.Component.Renderless.FilePicker (Action(..), ChildSlots, Component, ComponentHTML, ComponentM, ComponentRender, Input, Output(..), Query(..), Slot, State, StateStore, component, handleAction, handleQuery, initialState) as FilePicker
import Magento.Import.UI.Component.Renderless.FilePicker.Setters (Props, setProps) as FilePicker
import Type.Data.Symbol (SProxy(..))

_filePicker = SProxy :: SProxy "filePicker"

render :: forall m. MonadAff m => FilePicker.ComponentRender m
render _ =
  HH.input
  (FilePicker.setProps [])
