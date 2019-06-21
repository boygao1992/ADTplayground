module Adhoc.Cell where

import Prelude
import Renderless.State

import CSS as CSS
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
import Halogen.Util (debug)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as HES
import Web.DOM.Element as WDE
import Web.Event.Event as WE
import Web.HTML as WH
import Web.HTML.HTMLElement as WHE
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent.EventTypes as MET

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

  , enum :: Boolean
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
  -- , itemCount: 0

  , candidates: []
  , available: []
  , enum: false
  }

data Action
  = Edit
  | OnValueInput String
  | OnKeyDown KE.KeyboardEvent
  | OnBlurInput -- TODO replace by ??

  -- | Init
  -- | OnMouseDownWindow
  -- | OnMouseDownRoot
  -- | OnMouseUpRoot
  | OnMouseDownToggle
  -- | OnFocusInput
  -- | OnKeyDown -- TODO merge
  | OnClickItem Int
  | OnMouseEnterItem Int
  -- | OnValueInput -- TODO merge
  -- | Receive

data Query a
  = GetValue (String -> a)

type Input m =
  { render :: ComponentRender m
  , value :: String
  , available :: Array String
  , enum :: Boolean
  }

type Output = Void

type ChildSlots = ()

type ComponentM m a = H.HalogenM (StateStore m) Action ChildSlots Output m a
type Component m = H.Component HH.HTML Query (Input m) Output m
type ComponentHTML m = H.ComponentHTML Action ChildSlots m
type ComponentRender m = State -> ComponentHTML m

type Slot = H.Slot Query Output

inputRef = H.RefLabel "input" :: H.RefLabel
ghostRef = H.RefLabel "auto-size-input-ghost" :: H.RefLabel
-- inputRef = H.RefLabel "__nselect_input" :: H.RefLabel
menuRef = H.RefLabel "__nselect_menu" :: H.RefLabel
selectedItemRef = H.RefLabel "__nselect_selectedItem" :: H.RefLabel

component :: forall m. MonadAff m => Component m
component = H.mkComponent
  { initialState: \({ render, value, available, enum }) ->
     store render defaultInitialState
      { value = value
      , available = available
      , enum = enum
      }
  , render: extract
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      -- , initialize = Just Init
      }
  }

defaultRender :: forall m. MonadAff m => ComponentRender m
defaultRender { editing, value, cache, width, clickedInside, isOpen, highlightedIndex, candidates, available } =
  if editing
  then
    HH.div
      [
      -- [ HE.onMouseDown $ Just <<< const OnMouseDownRoot
      -- , HE.onMouseUp $ Just <<< const OnMouseUpRoot
      ]
    -- [ pure $
    [ HH.input  [ HP.value cache
                , HE.onKeyDown $ Just <<< OnKeyDown
                , HE.onValueInput $ Just <<< OnValueInput
                , HP.ref inputRef
                , HC.style do
                    CSS.key (CSS.fromString "border") "none"
                    CSS.padding (CSS.px 0.0) (CSS.px 0.0) (CSS.px 0.0) (CSS.px 0.0)
                    CSS.width (CSS.px width)

                -- , HE.onFocus $ Just <<< const OnFocusInput
                , HE.onBlur $ Just <<< const OnBlurInput
                ]
    -- , guard isOpen $>
    , if isOpen
      then
        HH.ul [ HP.ref menuRef
              , HC.style do
                  CSS.position CSS.absolute
                  CSS.overflowY CSS.overflowAuto
                  CSS.maxHeight (CSS.rem 10.0)

              , HP.class_ $ HH.ClassName "shadow-md bg-white"
              ]
        (Array.mapWithIndex renderItem candidates)
      else
        HH.text ""

    -- , pure $
    , HH.p  [ HC.style do
                CSS.display CSS.inlineBlock
                CSS.height CSS.nil
                CSS.overflow CSS.hidden
                CSS.position CSS.absolute
                CSS.top CSS.nil
                CSS.visibility CSS.visibilityHidden
                CSS.textWhitespace CSS.whitespaceNoWrap
            , HP.ref ghostRef
            ]
      [ HH.text cache ]
    ]
  else
    HH.p [ HE.onClick $ Just <<< const Edit ]
    [ HH.text value ]

  where
    renderItem index item =
      HH.li (
        [ HE.onClick $ Just <<< const (OnClickItem index)
        , HE.onMouseEnter $ Just <<< const (OnMouseEnterItem index)

        , HP.class_ $ HH.ClassName $ "py-1 px-3 cursor-pointer"
            <> Monoid.guard (index == highlightedIndex) " bg-blue-300"
        ]
        <> if (index == highlightedIndex)
            then [HP.ref selectedItemRef]
            else []
        )
      [ HH.text item ]

handleAction :: forall m. MonadAff m => Action -> ComponentM m Unit
handleAction = case _ of
  -- Init -> do
  --   win <- H.liftEffect WH.window
  --   void $ H.subscribe $
  --     HES.eventListenerEventSource MET.mousedown (Window.toEventTarget win)
  --     (Just <<< const OnMouseDownWindow)

  Edit -> do
    modifyState_ \st -> st { editing = true, cache = st.value }
    focusInput
    updateInputWidth

  OnValueInput str -> do
    modifyState_ \st -> st
      { cache = str
      , candidates
        = Array.filter (String.contains (String.Pattern str)) st.available
      , isOpen = true
      }
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
              $ for_ (s.available !! s.highlightedIndex) \selection -> do
                modifyState_ _
                  { cache = selection
                  , isOpen = false
                  }
            updateInputWidth
          else do
            modifyState_ _ { editing = false, value = s.cache }

      "Escape" -> do
        H.liftEffect $ WE.preventDefault $ KE.toEvent ke
        isOpen <- getsState _.isOpen
        if isOpen
          then do
            modifyState_ _ { isOpen = false }
          else do
            modifyState_ _ { editing = false }

      _ -> do
        pure unit

  OnBlurInput -> do
    isOpen <- getsState _.isOpen
    if isOpen
      then do
        handleVisibilityChange false
        focusInput
      else do
        handleVisibilityChange false
        modifyState_ _ { editing = false }

  -- OnMouseDownWindow -> do
  --   debug "OnMouseDownWindow"

  --   s <- getState

  --   if (not s.clickedInside && s.isOpen)
  --     then do
  --       handleVisibilityChange false
  --       focusInput
  --     else if (not s.isOpen && s.editing )
  --       then modifyState_ _ { editing = false }
  --       else pure unit

  -- OnMouseDownRoot -> do
  --   debug "OnMouseDownRoot"
  --   modifyState_ _ { clickedInside = true }

  -- OnMouseUpRoot -> do
  --   debug "OnMouseUpRoot"
  --   modifyState_ _ { clickedInside = false }

  OnMouseDownToggle -> do
    isOpen <- getsState _.isOpen
    handleVisibilityChange $ not isOpen

  -- OnFocusInput -> do
  --   handleVisibilityChange true

  OnClickItem index -> do
    available <- getsState _.available
    for_ (available !! index) \selection -> do
      modifyState_ _
        { cache = selection
        , isOpen = false
        }
    updateInputWidth

  OnMouseEnterItem index -> do
    modifyState_ _ { highlightedIndex = index }

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
        H.lift $ debug "Focus Input"
        H.lift $ H.liftEffect $ WHE.focus el

    -- blurInput :: ComponentM m Unit
    -- blurInput =
    --   void $ runMaybeT do
    --     el <- MaybeT $ H.getHTMLElementRef inputRef
    --     H.lift $ H.liftEffect $ WHE.blur el

    handleVisibilityChange isOpen = do
      state <- modifyState _ { isOpen = isOpen }

      -- Make sure highlighted item is visible when dropdown becomes open.
      when isOpen $
        scrollIntoViewIfNeeded state.highlightedIndex

    handleHighlightedIndexChange index = do
      modifyState_ _ { highlightedIndex = index }
      scrollIntoViewIfNeeded index

    scrollIntoViewIfNeeded index = do
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
