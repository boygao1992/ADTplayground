module NSelect.Setters where

import Prelude
import NSelect (Action(..), KeyDownHandler, inputRef, menuRef, selectedItemRef)

import Control.MonadPlus (guard)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event as Event
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME
import Halogen.HTML.CSS (style) as HC
import CSS (absolute, position) as CSS
import CSS.Overflow (overflowY, overflowAuto) as CSS

type RootProps r =
  ( onMouseDown :: ME.MouseEvent
  , onMouseUp :: ME.MouseEvent
  | r
  )

-- Click outside the root will close the dropdown.
setRootProps
  :: forall pa cs m r
  . Array (HH.IProp (RootProps r) (Action pa cs m))
  -> Array (HH.IProp (RootProps r) (Action pa cs m))
setRootProps props = props <>
  [ HE.onMouseDown $ Just <<< const OnMouseDownRoot
  , HE.onMouseUp $ Just <<< const OnMouseUpRoot
  ]

type ToggleProps r =
  ( onMouseDown :: ME.MouseEvent
  | r
  )

setToggleProps
  :: forall pa cs m r
  . Array (HH.IProp (ToggleProps r) (Action pa cs m))
  -> Array (HH.IProp (ToggleProps r) (Action pa cs m))
setToggleProps props = props <>
  [ HE.onMouseDown $ Just <<< const OnMouseDownToggle
  ]

type InputProps r =
  ( value :: String
  , onFocus :: FocusEvent
  , onKeyDown :: KE.KeyboardEvent
  , onInput :: Event.Event
  | r
  )

sharedInputProps
  :: forall pa cs m r
  . Array (HH.IProp (InputProps r) (Action pa cs m))
sharedInputProps =
  [ HP.ref inputRef
  , HE.onFocus $ Just <<< const OnFocusInput
  , HE.onValueInput $ Just <<< OnValueInput
  ]

setInputProps
  :: forall pa cs m r
  . Array (HH.IProp (InputProps r) (Action pa cs m))
  -> Array (HH.IProp (InputProps r) (Action pa cs m))
setInputProps props = props <> sharedInputProps <>
  [ HE.onKeyDown $ Just <<< OnKeyDownInput
  ]

-- | setInputProps' does everything setInputProps does, but also pass the
-- | keyboardEvent back to the parent component, so the parent can handle more
-- | key bindings like Tab.
setInputProps'
  :: forall pa cs m r
  . { onKeyDown :: KeyDownHandler pa }
  -> Array (HH.IProp (InputProps r) (Action pa cs m))
  -> Array (HH.IProp (InputProps r) (Action pa cs m))
setInputProps' parentHandlers props = props <> sharedInputProps <>
  [ HE.onKeyDown $ Just <<< OnKeyDownInput' parentHandlers.onKeyDown
  ]

type MenuProps r =
  ( style :: String
  | r
  )

-- | Use `setMenuProps` so that after ArrowUp/ArrowDown, highlighted item will
-- | still be visible.
setMenuProps
  :: forall pa cs m r
  . Array (HH.IProp (MenuProps r) (Action pa cs m))
  -> Array (HH.IProp (MenuProps r) (Action pa cs m))
setMenuProps props = props <>
  [ HP.ref menuRef
  , HC.style do
      CSS.position CSS.absolute
      CSS.overflowY CSS.overflowAuto
  ]

type ItemProps r =
  ( onClick :: ME.MouseEvent
  , onMouseEnter :: ME.MouseEvent
  | r
  )

setItemProps
  :: forall pa cs m r
  . { index :: Int, isSelected :: Boolean }
  -> Array (HH.IProp (ItemProps r) (Action pa cs m))
  -> Array (HH.IProp (ItemProps r) (Action pa cs m))
setItemProps { index, isSelected } props = props <>
  join
  [ guard isSelected $> HP.ref selectedItemRef
  , pure $ HE.onClick $ Just <<< const (OnClickItem index)
  , pure $ HE.onMouseEnter $ Just <<< const (OnMouseEnterItem index)
  ]
