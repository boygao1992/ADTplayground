module Adhoc.Cell.Setters where

import Prelude

import CSS (absolute, display, fromString, height, inlineBlock, key, maxHeight, nil, padding, position, px, rem, textWhitespace, top, whitespaceNoWrap, width) as CSS
import CSS.Overflow (hidden, overflow, overflowY, overflowAuto) as CSS
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

import Adhoc.Cell.Renderless (Action(..), ghostRef, inputRef, menuRef, selectedItemRef)

type InputProps p =
  ( value :: String
  , onKeyDown :: KE.KeyboardEvent
  , onInput :: WE.Event
  , onBlur :: FE.FocusEvent
  , style :: String
  | p
  )

setInputProps
  :: forall p m
  . { cache :: String, width :: Number }
  -> Array (HH.IProp (InputProps p) (Action m))
  -> Array (HH.IProp (InputProps p) (Action m))
setInputProps { cache, width } props = props <>
  [ HP.value cache
  , HE.onKeyDown $ Just <<< OnKeyDown
  , HE.onValueInput $ Just <<< OnValueInput
  , HE.onBlur $ Just <<< const OnBlurInput
  , HC.style do
      CSS.key (CSS.fromString "border") "none"
      CSS.padding (CSS.px 0.0) (CSS.px 0.0) (CSS.px 0.0) (CSS.px 0.0)
      CSS.width (CSS.px width)
  , HP.ref inputRef
  ]

type GhostElementProps p =
  ( style :: String
  | p
  )
setGhostElementProps
  :: forall p m
  . Array (HH.IProp (GhostElementProps p) (Action m))
  -> Array (HH.IProp (GhostElementProps p) (Action m))
setGhostElementProps props = props <>
  [ HC.style do
      CSS.display CSS.inlineBlock
      CSS.height CSS.nil
      CSS.overflow CSS.hidden
      CSS.position CSS.absolute
      CSS.top CSS.nil
      CSS.visibility CSS.visibilityHidden
      CSS.textWhitespace CSS.whitespaceNoWrap
  , HP.ref ghostRef
  ]

type DisplayProps p =
  ( onClick :: ME.MouseEvent
  | p
  )

setDisplayProps
  :: forall p m
  . Array (HH.IProp (DisplayProps p) (Action m))
  -> Array (HH.IProp (DisplayProps p) (Action m))
setDisplayProps props = props <>
  [ HE.onClick $ Just <<< const Edit ]

-- type ToggleProps p =
--   ( onMouseDown :: ME.MouseEvent
--   | p
--   )

-- setToggleProps
--   :: forall p m
--   . Array (HH.IProp (ToggleProps p) (Action m))
--   -> Array (HH.IProp (ToggleProps p) (Action m))
-- setToggleProps props = props <>
--   [ HE.onMouseDown $ Just <<< const OnMouseDownToggle
--   ]

type MenuProps p =
  ( style :: String
  | p
  )
setMenuProps
  :: forall p m
  . Array (HH.IProp (MenuProps p) (Action m))
  -> Array (HH.IProp (MenuProps p) (Action m))
setMenuProps props = props <>
  [ HC.style do
      CSS.position CSS.absolute
      CSS.overflowY CSS.overflowAuto
      CSS.maxHeight (CSS.rem 10.0) -- TODO configurable?
  , HP.ref menuRef
  ]

type ItemProps p =
  ( onClick :: ME.MouseEvent
  , onMouseEnter :: ME.MouseEvent
  | p
  )

setItemProps
  :: forall p m
  . { index :: Int, isSelected :: Boolean }
  -> Array (HH.IProp (ItemProps p) (Action m))
  -> Array (HH.IProp (ItemProps p) (Action m))
setItemProps { index, isSelected } props = props <>
  [ HE.onClick $ Just <<< const (OnClickItem index)
  , HE.onMouseEnter $ Just <<< const (OnMouseEnterItem index)
  ]
  <> if (isSelected)
     then [HP.ref selectedItemRef]
     else []
