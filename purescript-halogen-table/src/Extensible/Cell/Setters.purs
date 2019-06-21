module Extensible.Cell.Setters where

import Prelude

import CSS (absolute, display, fromString, height, inlineBlock, key, nil, padding, position, px, textWhitespace, top, whitespaceNoWrap, width) as CSS
import CSS.Overflow (hidden, overflow) as CSS
import CSS.Visibility (visibility, visibilityHidden) as CSS
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event as WE
import Web.UIEvent.FocusEvent as FE
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME

import Extensible.Cell.Renderless (Action, _ghost, _input, edit, onBlur, onKeyDown, onValueInput)

type InputProps r =
  ( value :: String
  , onKeyDown :: KE.KeyboardEvent
  , onInput :: WE.Event
  , onBlur :: FE.FocusEvent
  , style :: String
  | r
  )

setInputProps
  :: forall r a
  . { cache :: String, width :: Number }
  -> Array (HH.IProp (InputProps r) (Action a))
  -> Array (HH.IProp (InputProps r) (Action a))
setInputProps { cache, width } props = props <>
  [ HP.value cache
  , HE.onKeyDown $ Just <<< onKeyDown
  , HE.onValueInput $ Just <<< onValueInput
  , HE.onBlur $ Just <<< const onBlur
  , HP.ref _input
  , HC.style do
      CSS.key (CSS.fromString "border") "none"
      CSS.padding (CSS.px 0.0) (CSS.px 0.0) (CSS.px 0.0) (CSS.px 0.0)
      CSS.width (CSS.px width)
  ]

type GhostElementProps r =
  ( style :: String
  | r
  )

setGhostElementProps
  :: forall r a
  . Array (HH.IProp (GhostElementProps r) (Action a))
  -> Array (HH.IProp (GhostElementProps r) (Action a))
setGhostElementProps props = props <>
  [ HC.style do
      CSS.display CSS.inlineBlock
      CSS.height CSS.nil
      CSS.overflow CSS.hidden
      CSS.position CSS.absolute
      CSS.top CSS.nil
      CSS.visibility CSS.visibilityHidden
      CSS.textWhitespace CSS.whitespaceNoWrap
  , HP.ref _ghost
  ]


type DisplayProps r =
  ( onClick :: ME.MouseEvent
  | r
  )

setDisplayProps
  :: forall r a
  . Array (HH.IProp (DisplayProps r) (Action a))
  -> Array (HH.IProp (DisplayProps r) (Action a))
setDisplayProps props = props <>
  [ HE.onClick <<< const <<< Just $ edit
  ]
