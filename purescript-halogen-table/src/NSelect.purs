module NSelect where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Query.EventSource as ES
import Web.DOM.Element as Element
import Web.Event.Event as Event
import Web.HTML as Web
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent.EventTypes as ET

inputRef = H.RefLabel "__nselect_input" :: H.RefLabel
menuRef = H.RefLabel "__nselect_menu" :: H.RefLabel
selectedItemRef = H.RefLabel "__nselect_selectedItem" :: H.RefLabel

type KeyDownHandler pa = KE.KeyboardEvent -> pa

type State =
  { isOpen :: Boolean
  , highlightedIndex :: Int
  }

type InnerState pa cs m =
  { props :: Input pa cs m
  , clickedInside :: Boolean
  , isOpen :: Boolean
  , highlightedIndex :: Int
  }

initialState :: forall pa cs m. Input pa cs m -> InnerState pa cs m
initialState props =
  { props
  , clickedInside: false
  , isOpen: false
  , highlightedIndex: 0
  }

data Action pa cs m
  = Init
  | OnWindowMouseDown
  | OnMouseDownRoot
  | OnMouseUpRoot
  | OnMouseDownToggle
  | OnFocusInput
  | OnKeyDownInput KE.KeyboardEvent
  | OnKeyDownInput' (KeyDownHandler pa) KE.KeyboardEvent
  | OnClickItem Int
  | OnMouseEnterItem Int
  | OnValueInput String
  | Receive (Input pa cs m)
  | Raise pa

data Query a
  = Open a
  | Close a
  | Focus a
  | Highlight Int a
  | Select a
  | GetState (State -> a)
  | GetInputElement (HTMLElement -> a)

type Input pa cs m =
  { render :: State -> ComponentHTML pa cs m
  , itemCount :: Int
  }

data Output pa
  = Selected Int
  | InputValueChanged String
  | VisibilityChanged Boolean
  | Emit pa

stateAdapter :: forall pa cs m. InnerState pa cs m -> State
stateAdapter { isOpen, highlightedIndex } =
  { isOpen
  , highlightedIndex
  }

type ComponentHTML pa cs m = H.ComponentHTML (Action pa cs m) cs m
type ComponentRender pa cs m = State -> ComponentHTML pa cs m
type ComponentInnerRender pa cs m = InnerState pa cs m -> ComponentHTML pa cs m
type ComponentM pa cs m = H.HalogenM (InnerState pa cs m) (Action pa cs m) cs (Output pa) m
type Component pa cs m = H.Component HH.HTML Query (Input pa cs m) (Output pa) m

type Slot pa s = H.Slot Query (Output pa) s

component :: forall pa cs m . MonadAff m => Component pa cs m
component = H.mkComponent
  { initialState
  , render: innerRender
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , initialize = Just Init
      , receive = Just <<< Receive
      }
  }

innerRender :: forall pa cs m. ComponentInnerRender pa cs m
innerRender state = state.props.render $ stateAdapter state

handleVisibilityChange :: forall pa cs m.
  MonadEffect m => Boolean -> ComponentM pa cs m Unit
handleVisibilityChange isOpen = do
  state <- H.modify _ { isOpen = isOpen }

  -- Make sure highlighted item is visible when dropdown becomes open.
  when isOpen $
    scrollIntoViewIfNeeded state.highlightedIndex

  H.raise $ VisibilityChanged isOpen

handleHighlightedIndexChange :: forall pa cs m.
  MonadEffect m => Int -> ComponentM pa cs m Unit
handleHighlightedIndexChange index = do
  H.modify_ _ { highlightedIndex = index }
  scrollIntoViewIfNeeded index

scrollIntoViewIfNeeded :: forall pa cs m. MonadEffect m => Int -> ComponentM pa cs m Unit
scrollIntoViewIfNeeded index = do
  void $ runMaybeT do
    menu <- MaybeT $ H.getHTMLElementRef menuRef
    let menuEl = HTMLElement.toElement menu
    item <- MaybeT $ H.getHTMLElementRef selectedItemRef
    let itemEl = HTMLElement.toElement item

    H.lift $ H.liftEffect do
      itemOffsetTop <- HTMLElement.offsetTop item
      itemHeight <- Element.clientHeight itemEl
      scrollTop <- Element.scrollTop menuEl
      menuHeight <- Element.clientHeight menuEl
      let itemBelow = itemOffsetTop + itemHeight > scrollTop + menuHeight
          itemAbove = itemOffsetTop < scrollTop
      if itemBelow
        then Element.setScrollTop (itemOffsetTop + itemHeight - menuHeight) menuEl
        else if itemAbove
          then Element.setScrollTop itemOffsetTop menuEl
          else pure unit

handleAction :: forall pa cs m. MonadAff m => Action pa cs m -> ComponentM pa cs m Unit
handleAction = case _ of
  Init -> do
    win <- H.liftEffect Web.window
    void $ H.subscribe $
      ES.eventListenerEventSource ET.mousedown (Window.toEventTarget win)
        (Just <<< const OnWindowMouseDown)

  Receive props -> do
    H.modify_ _ { props = props }

  OnWindowMouseDown -> do
    state <- H.get
    when (not state.clickedInside && state.isOpen) $ do
      handleVisibilityChange false

  OnMouseDownRoot -> do
    H.modify_ _ { clickedInside = true }

  OnMouseUpRoot -> do
    H.modify_ _ { clickedInside = false }

  OnMouseDownToggle -> do
    state <- H.get
    handleVisibilityChange $ not state.isOpen

  OnFocusInput -> do
    handleVisibilityChange true

  OnKeyDownInput kbEvent -> do
    state <- H.get
    let event = KE.toEvent kbEvent
    when state.isOpen $ case KE.key kbEvent of
      "ArrowUp" -> do
        H.liftEffect $ Event.preventDefault event
        s <- H.get
        let nextIndex = max 0 (s.highlightedIndex - 1)
        when (nextIndex /= s.highlightedIndex) $
          handleHighlightedIndexChange nextIndex
      "ArrowDown" -> do
        H.liftEffect $ Event.preventDefault event
        s <- H.get
        let nextIndex = min (s.props.itemCount - 1) (s.highlightedIndex + 1)
        when (nextIndex /= s.highlightedIndex) $
          handleHighlightedIndexChange nextIndex
      "Enter" -> do
        s <- H.get
        when (s.props.itemCount > 0) $
          H.raise $ Selected s.highlightedIndex
      _ -> pure unit

  OnKeyDownInput' parentOnKeyDown kbEvent -> do
    handleAction (OnKeyDownInput kbEvent)
    H.raise $ Emit $ parentOnKeyDown kbEvent

  OnClickItem index -> do
    H.raise $ Selected index

  OnMouseEnterItem index -> do
    H.modify_ _ { highlightedIndex = index }

  OnValueInput value -> do
    handleHighlightedIndexChange 0
    H.raise $ InputValueChanged value

  Raise pa -> do
    H.raise $ Emit pa


handleQuery :: forall pa cs m a. MonadAff m => Query a -> ComponentM pa cs m (Maybe a)
handleQuery = case _ of
  Open n -> Just n <$ do
    handleVisibilityChange true

  Close n -> Just n <$ do
    handleVisibilityChange false

  Focus n -> Just n <$ do
    H.fork $ void $ runMaybeT do
      input <- MaybeT $ H.getHTMLElementRef inputRef
      H.lift $ H.liftEffect $ HTMLElement.focus input

  Highlight index n -> Just n <$ do
    handleHighlightedIndexChange index

  Select n -> Just n <$ do
    idx <- H.gets _.highlightedIndex
    H.raise $ Selected idx

  GetState reply -> do
    innerState <- H.gets stateAdapter
    pure $ Just $ reply innerState

  GetInputElement reply -> runMaybeT do
    el <- MaybeT $ H.getHTMLElementRef inputRef
    pure $ reply el

-- A helper function to redirect parent action through the `Emit` message, so
-- that parent component can handle it.
raise :: forall pa cs m. pa -> Action pa cs m
raise = Raise
