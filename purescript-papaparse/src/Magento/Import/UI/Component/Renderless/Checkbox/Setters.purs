module Magento.Import.UI.Component.Renderless.Checkbox.Setters where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Magento.Import.UI.Component.Renderless.Checkbox as Checkbox
import Web.Event.Event as WE

type Props r =
  ( type :: HP.InputType
  , onChange :: WE.Event
  , checked :: Boolean
  | r
  )

setProps
  :: forall r m
  . Boolean
  -> Array (HP.IProp (Props r) (Checkbox.Action m))
  -> Array (HP.IProp (Props r) (Checkbox.Action m))
setProps b = append
  [ HP.type_ HP.InputCheckbox
  , HE.onChange $ Just <<< const Checkbox.Toggle
  , HP.checked b
  ]
