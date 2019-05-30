module Magento.Import.UI.Component.FilePicker.Render where

import Effect.Aff.Class (class MonadAff)
import Halogen.HTML as HH
import Magento.Import.UI.Component.FilePicker.Renderless (ComponentRender) as FilePicker
import Magento.Import.UI.Component.FilePicker.Setters (setProps) as FilePicker
import Type.Data.Symbol (SProxy(..))

_filePicker = SProxy :: SProxy "filePicker"

simpleRender :: forall m. MonadAff m => FilePicker.ComponentRender m
simpleRender _ =
  HH.input
  (FilePicker.setProps [])
