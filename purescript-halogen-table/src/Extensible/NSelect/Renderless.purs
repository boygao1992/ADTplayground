module Extensible.NSelect.Renderless where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Maybe (Maybe(..))
import Data.Variant as V
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Query.EventSource as HES
import Renderless.State (Store, extract, getState, getsState, modifyState, modifyState_, store)
import Type.Data.Symbol (SProxy(..))
import Type.Row (type (+))
import Web.DOM.Element as WDE
import Web.Event.Event as WE
import Web.HTML as WH
import Web.HTML.HTMLElement as WHE
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent.EventTypes as MET

type StateRow =
  ( isOpen :: Boolean
  , highlightedIndex :: Int
  )

type State = Record StateRow

type InnerState =
  { props :: Props
  , clickedInside :: Boolean
  | StateRow
  }

type StateStore pa cs m = Store InnerState (ComponentHTML pa cs m)

initialInnerState :: Props -> InnerState
initialInnerState props =
  { props
  , clickedInside: false
  , isOpen: false
  , highlightedIndex: 0
  }

innerStateToState :: InnerState -> State
innerStateToState { isOpen, highlightedIndex } = { isOpen, highlightedIndex }

---------
-- Action

type Action pa = V.Variant (ActionRow pa)
type Action' = V.Variant (ActionRow ())

type ActionRow pa
  = Init
  + OnWindowMouseDown
  + OnMouseDownRoot
  + OnMouseUpRoot
  + OnMouseDownToggle
  + OnFocusInput
  + OnKeyDownInput
  + OnClickItem
  + OnMouseEnterItem
  + OnValueInput
  + Receive
  + pa

handleAction :: forall pa cs m. MonadAff m => Action pa -> ComponentM pa cs m Unit
handleAction
  = V.on _init handleInit
  <<< V.on _onWindowMouseDown handleOnWindowMouseDown
  <<< V.on _onMouseDownRoot handleOnMouseDownRoot
  <<< V.on _onMouseUpRoot handleOnMouseUpRoot
  <<< V.on _onMouseDownToggle handleOnMouseDownToggle
  <<< V.on _onFocusInput handleOnFocusInput
  <<< V.on _onKeyDownInput handleOnKeyDownInput
  <<< V.on _onClickItem handleOnClickItem
  <<< V.on _onMouseEnterItem handleOnMouseEnterItem
  <<< V.on _onValueInput handleOnValueInput
  <<< V.on _receive handleReceive
  $ H.raise <<< RaiseUnhandled

-- | Action.Utils
handleVisibilityChange :: forall pa cs m.
  MonadEffect m => Boolean -> ComponentM pa cs m Unit
handleVisibilityChange isOpen = do
  state <- modifyState _ { isOpen = isOpen }

  -- Make sure highlighted item is visible when dropdown becomes open.
  when isOpen $
    scrollIntoViewIfNeeded state.highlightedIndex

  H.raise $ VisibilityChanged isOpen

handleHighlightedIndexChange :: forall pa cs m.
  MonadEffect m => Int -> ComponentM pa cs m Unit
handleHighlightedIndexChange index = do
  modifyState_ _ { highlightedIndex = index }
  scrollIntoViewIfNeeded index

scrollIntoViewIfNeeded :: forall pa cs m. MonadEffect m => Int -> ComponentM pa cs m Unit
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

-- | Action > Init
type Init a = ( nSelectInit :: Unit | a)
_init = SProxy :: SProxy "nSelectInit"
init :: forall a. V.Variant (Init a)
init = V.inj _init unit

handleInit :: forall pa cs m. MonadAff m => Unit -> ComponentM pa cs m Unit
handleInit _ = do
  win <- H.liftEffect WH.window
  void $ H.subscribe $
    HES.eventListenerEventSource MET.mousedown (Window.toEventTarget win)
    (Just <<< const onWindowMouseDown)

-- | Action > OnWindowMouseDown
type OnWindowMouseDown a = ( nSelectOnWindowMouseDown :: Unit | a)
_onWindowMouseDown = SProxy :: SProxy "nSelectOnWindowMouseDown"
onWindowMouseDown :: forall a. V.Variant (OnWindowMouseDown a)
onWindowMouseDown = V.inj _onWindowMouseDown unit

handleOnWindowMouseDown :: forall pa cs m. MonadEffect m => Unit -> ComponentM pa cs m Unit
handleOnWindowMouseDown _ = do
  clickedInside <- getsState _.clickedInside
  isOpen <- getsState _.isOpen
  when (not clickedInside && isOpen) $ do
    handleVisibilityChange false

-- | Action > OnMouseDownRoot
type OnMouseDownRoot a = ( nSelectOnMouseDownRoot :: Unit | a)
_onMouseDownRoot = SProxy :: SProxy "nSelectOnMouseDownRoot"
onMouseDownRoot :: forall a. V.Variant (OnMouseDownRoot a)
onMouseDownRoot = V.inj _onMouseDownRoot unit

handleOnMouseDownRoot :: forall pa cs m. Unit -> ComponentM pa cs m Unit
handleOnMouseDownRoot _ = do
  modifyState_ _ { clickedInside = true }

-- | Action > OnMouseUpRoot
type OnMouseUpRoot a = ( nSelectOnMouseUpRoot :: Unit | a)
_onMouseUpRoot = SProxy :: SProxy "nSelectOnMouseUpRoot"
onMouseUpRoot :: forall a. V.Variant (OnMouseUpRoot a)
onMouseUpRoot = V.inj _onMouseUpRoot unit

handleOnMouseUpRoot :: forall pa cs m. Unit -> ComponentM pa cs m Unit
handleOnMouseUpRoot _ = do
  modifyState_ _ { clickedInside = false }

-- | Action > OnMouseDownToggle
type OnMouseDownToggle a = ( nSelectOnMouseDownToggle :: Unit | a)
_onMouseDownToggle = SProxy :: SProxy "nSelectOnMouseDownToggle"
onMouseDownToggle :: forall a. V.Variant (OnMouseDownToggle a)
onMouseDownToggle = V.inj _onMouseDownToggle unit

handleOnMouseDownToggle :: forall pa cs m. MonadEffect m => Unit -> ComponentM pa cs m Unit
handleOnMouseDownToggle _ = do
  isOpen <- getsState _.isOpen
  handleVisibilityChange $ not isOpen

-- | Action > OnFocusInput
type OnFocusInput a = ( nSelectOnFocusInput :: Unit | a)
_onFocusInput = SProxy :: SProxy "nSelectOnFocusInput"
onFocusInput :: forall a. V.Variant (OnFocusInput a)
onFocusInput = V.inj _onFocusInput unit

handleOnFocusInput :: forall pa cs m. MonadEffect m => Unit -> ComponentM pa cs m Unit
handleOnFocusInput _ = do
  handleVisibilityChange true

-- | Action > OnKeyDownInput KE.KeyboardEvent
type OnKeyDownInput a = ( nSelectOnKeyDownInput :: KE.KeyboardEvent | a)
_onKeyDownInput = SProxy :: SProxy "nSelectOnKeyDownInput"
onKeyDownInput :: forall a. KE.KeyboardEvent -> V.Variant (OnKeyDownInput a)
onKeyDownInput = V.inj _onKeyDownInput

handleOnKeyDownInput :: forall pa cs m. MonadEffect m => KE.KeyboardEvent -> ComponentM pa cs m Unit
handleOnKeyDownInput kbEvent = do
  isOpen <- getsState _.isOpen
  let event = KE.toEvent kbEvent
  when isOpen $ case KE.key kbEvent of
    "ArrowUp" -> do
      H.liftEffect $ WE.preventDefault event
      s <- getState
      let nextIndex = max 0 (s.highlightedIndex - 1)
      when (nextIndex /= s.highlightedIndex) $
        handleHighlightedIndexChange nextIndex
    "ArrowDown" -> do
      H.liftEffect $ WE.preventDefault event
      s <- getState
      let nextIndex = min (s.props.itemCount - 1) (s.highlightedIndex + 1)
      when (nextIndex /= s.highlightedIndex) $
        handleHighlightedIndexChange nextIndex
    "Enter" -> do
      s <- getState
      when (s.props.itemCount > 0) $
        H.raise $ Selected s.highlightedIndex
    _ -> pure unit

-- | Action > OnKeyDownInput' (KeyDownHandler pa) KE.KeyboardEvent
-- NOTE no longer needed, unhandled actions registered by ancestors will be raised by default

-- | Action > OnClickItem Int
type OnClickItem a = ( nSelectOnClickItem :: Int | a)
_onClickItem = SProxy :: SProxy "nSelectOnClickItem"
onClickItem :: forall a. Int -> V.Variant (OnClickItem a)
onClickItem = V.inj _onClickItem

handleOnClickItem :: forall pa cs m. Int -> ComponentM pa cs m Unit
handleOnClickItem index = do
  H.raise $ Selected index

-- | Action > OnMouseEnterItem Int
type OnMouseEnterItem a = ( nSelectOnMouseEnterItem :: Int | a)
_onMouseEnterItem = SProxy :: SProxy "nSelectOnMouseEnterItem"
onMouseEnterItem :: forall a. Int -> V.Variant (OnMouseEnterItem a)
onMouseEnterItem = V.inj _onMouseEnterItem

handleOnMouseEnterItem :: forall pa cs m. Int -> ComponentM pa cs m Unit
handleOnMouseEnterItem index = do
  modifyState_ _ { highlightedIndex = index }

-- | Action > OnValueInput String
type OnValueInput a = ( nSelectOnValueInput :: String | a)
_onValueInput = SProxy :: SProxy "nSelectOnValueInput"
onValueInput :: forall a. String -> V.Variant (OnValueInput a)
onValueInput = V.inj _onValueInput

handleOnValueInput :: forall pa cs m. MonadEffect m => String -> ComponentM pa cs m Unit
handleOnValueInput value = do
  handleHighlightedIndexChange 0
  H.raise $ InputValueChanged value

-- | Action > Receive (Input pa cs m)
type Receive a = ( nSelectReceive :: Props | a)
_receive = SProxy :: SProxy "nSelectReceive"
receive :: forall a. Props -> V.Variant (Receive a)
receive = V.inj _receive

handleReceive :: forall pa cs m. Props -> ComponentM pa cs m Unit
handleReceive props = do
  modifyState_ _ { props = props }

-- | Action > Raise pa
-- NOTE no longer needed, unhandled actions registered by ancestors will be raised by default

--------
-- Query

data Query a
  = Open a
  | Close a
  | Focus a
  | Highlight Int a
  | Select a
  | GetState (State -> a)
  | GetInputElement (WHE.HTMLElement -> a)

handleQuery :: forall pa cs m a. MonadAff m => Query a -> ComponentM pa cs m (Maybe a)
handleQuery = case _ of
  Open n -> Just n <$ do
    handleVisibilityChange true

  Close n -> Just n <$ do
    handleVisibilityChange false

  Focus n -> Just n <$ do
    H.fork $ void $ runMaybeT do
      input <- MaybeT $ H.getHTMLElementRef inputRef
      H.lift $ H.liftEffect $ WHE.focus input

  Highlight index n -> Just n <$ do
    handleHighlightedIndexChange index

  Select n -> Just n <$ do
    idx <- getsState _.highlightedIndex
    H.raise $ Selected idx

  GetState reply -> do
    innerState <- getsState innerStateToState
    pure $ Just $ reply innerState

  GetInputElement reply -> runMaybeT do
    el <- MaybeT $ H.getHTMLElementRef inputRef
    pure $ reply el

--------
-- Input

type Input pa cs m =
  { render :: ComponentRender pa cs m
  | PropsRow
  }

type PropsRow =
  ( itemCount :: Int
  )

type Props = Record PropsRow

inputToProps :: forall pa cs m. Input pa cs m -> Props
inputToProps { itemCount } = { itemCount }

---------
-- Output

data Output a
  = Selected Int
  | InputValueChanged String
  | VisibilityChanged Boolean
  | RaiseUnhandled (V.Variant a)

--------
-- Types
type ComponentHTML' m = ComponentHTML () () m
type ComponentRender' m = ComponentRender () () m
type Component' m = Component () () m

type ComponentHTML pa cs m = H.ComponentHTML (Action pa) cs m
type ComponentRender pa cs m = State -> ComponentHTML pa cs m
type ComponentInnerRender pa cs m = InnerState -> ComponentHTML pa cs m
type ComponentM pa cs m = H.HalogenM (StateStore pa cs m) (Action pa) cs (Output pa) m
type Component pa cs m = H.Component HH.HTML Query (Input pa cs m) (Output pa) m

type Slot pa s = H.Slot Query (Output pa) s

------------
-- Component

component :: forall pa cs m. MonadAff m => Component pa cs m
component = H.mkComponent
  { initialState: \({render, itemCount}) ->
     store (render <<< innerStateToState) (initialInnerState {itemCount})
  , render: extract
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , initialize = Just init
      , receive = Just <<< receive <<< inputToProps
      }
  }

----------
-- Setters

inputRef = H.RefLabel "__nselect_input" :: H.RefLabel
menuRef = H.RefLabel "__nselect_menu" :: H.RefLabel
selectedItemRef = H.RefLabel "__nselect_selectedItem" :: H.RefLabel
