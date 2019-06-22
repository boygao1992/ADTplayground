module Adhoc.Cell where

import Prelude

import CSS (absolute, display, fromString, height, inlineBlock, key, maxHeight, nil, padding, position, px, rem, textWhitespace, top, whitespaceNoWrap, width) as CSS
import CSS.Overflow (hidden, overflow, overflowY, overflowAuto) as CSS
import CSS.Visibility (visibility, visibilityHidden) as CSS
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array ((!!))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Monoid (guard) as Monoid
import Data.String as String
import Data.Traversable (for_)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Renderless.State (Store, extract, getState, getsState, modifyState_, modifyStore_, store)
import Web.DOM.Element as WDE
import Web.Event.Event as WE
import Web.HTML.HTMLElement as WHE
import Web.UIEvent.FocusEvent as FE
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME


type State =
  { editing :: Boolean
  , value :: String
  , cache :: String
  , width :: Number

  , clickedInside :: Boolean
  , isOpen :: Boolean
  , highlightedIndex :: Int

  , candidates :: Array String
  , available :: Array String

  , displayMessage :: Boolean
  , isEnum :: Boolean
  }

type StateStore m = Store State (ComponentHTML m)

defaultInitialState :: State
defaultInitialState =
  { editing: false
  , value: ""
  , cache: ""
  , width: 0.0

  , clickedInside: false
  , isOpen: false
  , highlightedIndex: 0

  , candidates: []
  , available: []

  , displayMessage: false
  , isEnum: false
  }

data Action m
  = Edit
  | OnValueInput String
  | OnKeyDown KE.KeyboardEvent
  | OnBlurInput -- TODO replace by ??
  | OnMouseDownToggle
  | OnClickItem Int
  | OnMouseEnterItem Int
  | Receive (Input m)

data Query a
  = GetValue (String -> a)

type Input m =
  { render :: ComponentRender m
  -- External State
  , available :: Array String
  -- Config
  , value :: String
  , isEnum :: Boolean
  }

type Output = Void

type ChildSlots = ()

type ComponentM m a = H.HalogenM (StateStore m) (Action m) ChildSlots Output m a
type Component m = H.Component HH.HTML Query (Input m) Output m
type ComponentHTML m = H.ComponentHTML (Action m) ChildSlots m
type ComponentRender m = State -> ComponentHTML m

type Slot = H.Slot Query Output

inputRef = H.RefLabel "__cell_input" :: H.RefLabel
ghostRef = H.RefLabel "__cell_ghostEl" :: H.RefLabel
menuRef = H.RefLabel "__nselect_menu" :: H.RefLabel
selectedItemRef = H.RefLabel "__nselect_selectedItem" :: H.RefLabel

component :: forall m. MonadAff m => Component m
component = H.mkComponent
  { initialState: \({ render, value, available, isEnum }) ->
     store render defaultInitialState
      { value = value
      , available = available
      , isEnum = isEnum
      }
  , render: extract
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , receive = Just <<< Receive
      }
  }

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

type ToggleProps p =
  ( onMouseDown :: ME.MouseEvent
  | p
  )

setToggleProps
  :: forall p m
  . Array (HH.IProp (ToggleProps p) (Action m))
  -> Array (HH.IProp (ToggleProps p) (Action m))
setToggleProps props = props <>
  [ HE.onMouseDown $ Just <<< const OnMouseDownToggle
  ]

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


defaultRender :: forall m. MonadAff m => ComponentRender m
defaultRender { editing, value, cache, width, isOpen, highlightedIndex, candidates, displayMessage } =
  if editing
  then
    HH.div []
    [ HH.input $ setInputProps { cache, width } []
    , if displayMessage
      then
        HH.p [ HP.class_ $ HH.ClassName "text-red-600" ]
        [ HH.text "not an valid input" ]
      else HH.text ""
    , if isOpen
      then
        HH.ul ( setMenuProps
                [ HP.class_ $ HH.ClassName "shadow-md bg-white"
                ])
        $ Array.mapWithIndex renderItem candidates
      else
        HH.text ""
    , HH.p (setGhostElementProps [])
      [ HH.text cache ]
    ]
  else
    HH.p (setDisplayProps [])
    [ HH.text value ]

  where
    renderItem index item =
      HH.li ( setItemProps
              { index
              , isSelected: index == highlightedIndex
              }
              [ HP.class_ $ HH.ClassName $ "py-1 px-3 cursor-pointer"
                  <> Monoid.guard (index == highlightedIndex) " bg-blue-300"
              ])
      [ HH.text item ]

handleAction :: forall m. MonadAff m => Action m -> ComponentM m Unit
handleAction = case _ of
  Edit -> do
    modifyState_ \st -> st { editing = true, cache = st.value }
    focusInput
    updateInputWidth

  OnValueInput str -> do
    available <- getsState _.available
    let candidates = Array.filter (String.contains (String.Pattern str)) available
    modifyState_ \st -> st
      { cache = str
      , candidates = candidates
      -- , highlightedIndex = 0
      , displayMessage = false
      }
    handleMenuVisibilityChange $ Array.length candidates > 0
    updateInputWidth

  OnKeyDown ke -> do
    case KE.key ke of
      "ArrowUp" -> do
        H.liftEffect $ WE.preventDefault $ KE.toEvent ke
        s <- getState
        let nextIndex = max 0 (s.highlightedIndex - 1)
        when (nextIndex /= s.highlightedIndex) $
        handleHighlightedIndexChange nextIndex
      "ArrowDown" -> do
        H.liftEffect $ WE.preventDefault $ KE.toEvent ke
        s <- getState
        let nextIndex = min (Array.length s.candidates - 1) (s.highlightedIndex + 1)
        when (nextIndex /= s.highlightedIndex) $
          handleHighlightedIndexChange nextIndex
      "Enter" -> do
        H.liftEffect $ WE.preventDefault $ KE.toEvent ke
        s <- getState
        if s.isOpen
          then do
            when (Array.length s.candidates > 0)
              $ for_ (s.candidates !! s.highlightedIndex) \selection -> do
                modifyState_ _ { cache = selection }
                handleMenuVisibilityChange false
                updateInputWidth
          else do
            if (s.isEnum && not Array.elem s.cache s.available)
              then do
                modifyState_ _ { displayMessage = true }
              else do
                modifyState_ _ { editing = false, value = s.cache }

      "Escape" -> do
        H.liftEffect $ WE.preventDefault $ KE.toEvent ke
        isOpen <- getsState _.isOpen
        -- if isOpen
        --   then do
        --     handleMenuVisibilityChange false
        --   else do
        --     modifyState_ _ { editing = false }
        handleMenuVisibilityChange false
        modifyState_ _ { editing = false }

      _ -> do
        pure unit

  OnBlurInput -> do
    isOpen <- getsState _.isOpen
    if isOpen
      then do
        handleMenuVisibilityChange false
        focusInput
      else do
        handleMenuVisibilityChange false
        modifyState_ _ { editing = false }

  OnMouseDownToggle -> do
    isOpen <- getsState _.isOpen
    handleMenuVisibilityChange $ not isOpen

  OnClickItem index -> do
    available <- getsState _.available
    for_ (available !! index) \selection -> do
      modifyState_ _ { cache = selection }
      handleMenuVisibilityChange false
      updateInputWidth

  OnMouseEnterItem index -> do
    modifyState_ _ { highlightedIndex = index }

  Receive { render, available } -> do
    modifyStore_ render (_ { available = available })

  where
    updateInputWidth :: ComponentM m Unit
    updateInputWidth =
      void $ runMaybeT do
        ghostEl <- MaybeT $ H.getHTMLElementRef ghostRef
        textWidth
          <- H.lift <<< H.liftEffect
          $ map _.width
          $ WHE.getBoundingClientRect
          $ ghostEl
        H.lift $ modifyState_ _ { width = textWidth }

    focusInput :: ComponentM m Unit
    focusInput =
      void $ runMaybeT do
        el <- MaybeT $ H.getHTMLElementRef inputRef
        H.lift $ H.liftEffect $ WHE.focus el

    handleMenuVisibilityChange isOpen = do
      modifyState_ _ { isOpen = isOpen }
      -- Make sure highlighted item is visible when dropdown becomes open.
      when isOpen $
        scrollIntoViewIfNeeded

    handleHighlightedIndexChange index = do
      modifyState_ _ { highlightedIndex = index }
      scrollIntoViewIfNeeded

    scrollIntoViewIfNeeded = do
      void $ runMaybeT do
        menu <- MaybeT $ H.getHTMLElementRef menuRef
        let menuEl = WHE.toElement menu
        item <- MaybeT $ H.getHTMLElementRef selectedItemRef
        let itemEl = WHE.toElement item

        H.lift $ H.liftEffect do
          itemOffsetTop <- WHE.offsetTop item
          itemHeight <- WDE.clientHeight itemEl
          scrollTop <- WDE.scrollTop menuEl
          menuHeight <- WDE.clientHeight menuEl
          let itemBelow = itemOffsetTop + itemHeight > scrollTop + menuHeight
              itemAbove = itemOffsetTop < scrollTop
          if itemBelow
            then WDE.setScrollTop (itemOffsetTop + itemHeight - menuHeight) menuEl
            else if itemAbove
                then WDE.setScrollTop itemOffsetTop menuEl
                else pure unit

handleQuery :: forall m a. MonadAff m => Query a -> ComponentM m (Maybe a)
handleQuery = case _ of
  GetValue reply -> do
    value <- getsState _.value
    pure $ Just <<< reply $ value
