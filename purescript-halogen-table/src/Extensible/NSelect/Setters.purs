module Extensible.NSelect.Setters where

import Prelude

import CSS (absolute, position) as CSS
import CSS.Overflow (overflowY, overflowAuto) as CSS

import Control.MonadPlus (guard)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event as WE
import Web.UIEvent.FocusEvent as FE
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME

import Extensible.NSelect.Renderless (Action, menuRef, onClickItem, onFocusInput, onKeyDownInput, onMouseDownRoot, onMouseDownToggle, onMouseEnterItem, onMouseUpRoot, onValueInput, selectedItemRef)

type RootProps r =
  ( onMouseDown :: ME.MouseEvent
  , onMouseUp :: ME.MouseEvent
  | r
  )

-- Click outside the root will close the dropdown.
setRootProps
  :: forall p pa
  . Array (HH.IProp (RootProps p) (Action pa))
  -> Array (HH.IProp (RootProps p) (Action pa))
setRootProps props = props <>
  [ HE.onMouseDown $ Just <<< const onMouseDownRoot
  , HE.onMouseUp $ Just <<< const onMouseUpRoot
  ]

type ToggleProps p =
  ( onMouseDown :: ME.MouseEvent
  | p
  )

setToggleProps
  :: forall p pa
  . Array (HH.IProp (ToggleProps p) (Action pa))
  -> Array (HH.IProp (ToggleProps p) (Action pa))
setToggleProps props = props <>
  [ HE.onMouseDown $ Just <<< const onMouseDownToggle
  ]

type InputProps p =
  ( value :: String
  , onFocus :: FE.FocusEvent
  , onKeyDown :: KE.KeyboardEvent
  , onInput :: WE.Event
  | p
  )

sharedInputProps
  :: forall p pa
  . Array (HH.IProp (InputProps p) (Action pa))
sharedInputProps =
  [ HE.onFocus $ Just <<< const onFocusInput
  , HE.onValueInput $ Just <<< onValueInput
  ]

setInputProps
  :: forall p pa
  . Array (HH.IProp (InputProps p) (Action pa))
  -> Array (HH.IProp (InputProps p) (Action pa))
setInputProps props = props <> sharedInputProps <>
  [ HE.onKeyDown $ Just <<< onKeyDownInput
  ]

type MenuProps p =
  ( style :: String
  | p
  )

-- | Use `setMenuProps` so that after ArrowUp/ArrowDown, highlighted item will
-- | still be visible.
setMenuProps
  :: forall p pa
  . Array (HH.IProp (MenuProps p) (Action pa))
  -> Array (HH.IProp (MenuProps p) (Action pa))
setMenuProps props = props <>
  [ HP.ref menuRef
  , HC.style do
      CSS.position CSS.absolute
      CSS.overflowY CSS.overflowAuto
  ]

type ItemProps p =
  ( onClick :: ME.MouseEvent
  , onMouseEnter :: ME.MouseEvent
  | p
  )

setItemProps
  :: forall p pa
  . { index :: Int, isSelected :: Boolean }
  -> Array (HH.IProp (ItemProps p) (Action pa))
  -> Array (HH.IProp (ItemProps p) (Action pa))
setItemProps { index, isSelected } props = props <>
  join
  [ guard isSelected $> HP.ref selectedItemRef
  , pure $ HE.onClick $ Just <<< const (onClickItem index)
  , pure $ HE.onMouseEnter $ Just <<< const (onMouseEnterItem index)
  ]
