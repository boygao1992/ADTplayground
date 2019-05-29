module Magento.Import.UI.Component.Renderless.FilePicker.Setters where

import Prelude

import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Magento.Import.UI.Component.Renderless.FilePicker (Action(..))
import Web.Event.Event as WE
import Web.HTML.HTMLInputElement as HIE

type Props r =
  ( type :: HP.InputType
  , onChange :: WE.Event
  | r
  )

setProps
  :: forall r
  . Array (HP.IProp (Props r) Action)
  -> Array (HP.IProp (Props r) Action)
setProps = append
  [ HP.type_ HP.InputFile
  , HE.onChange $ map SetFile <<< HIE.fromEventTarget <=< WE.target
  ]
