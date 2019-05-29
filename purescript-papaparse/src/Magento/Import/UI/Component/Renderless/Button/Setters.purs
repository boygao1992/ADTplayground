module Magento.Import.UI.Component.Renderless.Button.Setters where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Magento.Import.UI.Component.Renderless.Button as Button
import Web.UIEvent.MouseEvent as ME

type Props r =
  ( onClick :: ME.MouseEvent
  | r
  )

setProps
  :: forall r
  . Array (HP.IProp (Props r) Button.Action)
  -> Array (HP.IProp (Props r) Button.Action)
setProps = append
  [ HE.onClick $ Just <<< const Button.OnClick ]
