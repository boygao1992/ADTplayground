module Magento.Import.UI.Component.Renderless.TextField.Setters where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Magento.Import.UI.Component.Renderless.TextField as TextField
import Web.Event.Event as WE

type Props r =
  ( type :: HP.InputType
  , onInput :: WE.Event
  , value :: String
  | r
  )

setProps
  :: forall m r
  . String
  -> Array (HP.IProp (Props r) (TextField.Action m))
  -> Array (HP.IProp (Props r) (TextField.Action m))
setProps str = append
  [ HP.type_ HP.InputText
  , HE.onValueInput $ Just <<< TextField.KeyboardInput
  , HP.value str
  ]
