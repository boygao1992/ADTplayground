module Magento.Import.UI.Component.Button.Setters where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Magento.Import.UI.Component.Button.Renderless as Button
import Web.UIEvent.MouseEvent as ME

type Props r =
  ( onClick :: ME.MouseEvent
  | r
  )

setProps
  :: forall m r
  . Array (HP.IProp (Props r) (Button.Action m))
  -> Array (HP.IProp (Props r) (Button.Action m))
setProps = append
  [ HE.onClick $ Just <<< const Button.OnClick ]
