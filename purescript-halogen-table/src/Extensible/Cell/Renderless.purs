module Extensible.Cell.Renderless where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Maybe (Maybe(..))
import Data.Variant as V
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Renderless.State (Store, extract, getsState, modifyState_, store)
import Type.Data.Symbol (SProxy(..))
import Type.Row (type (+))
import Web.Event.Event as WE
import Web.HTML.HTMLElement as WHE
import Web.UIEvent.KeyboardEvent as KE

--------
-- State

type State =
  { editing :: Boolean
  , value :: String
  , cache :: String
  , width :: Number
  }

defaultInitialState :: State
defaultInitialState =
  { editing: false
  , value: ""
  , cache: ""
  , width: 0.0
  }

type StateStore pa cs m = Store State (ComponentHTML pa cs m)

---------
-- Action

type ActionRow a
  = Edit
  + OnValueInput
  + OnKeyDown
  + OnBlur
  + a

type Action a = V.Variant (ActionRow a)

handleAction :: forall pa cs m. MonadEffect m => Action pa -> ComponentM pa cs m Unit
handleAction
  = V.on _edit handleActionEdit
  <<< V.on _onBlur handleActionOnBlur
  <<< V.on _onKeyDown handleActionOnKeyDown
  <<< V.on _onValueInput handleActionOnValueInput
  $ H.raise <<< RaiseUnhandled

-- | Action.Utils

updateInputWidth :: forall pa cs m. MonadEffect m => ComponentM pa cs m Unit
updateInputWidth =
  void $ runMaybeT do
    ghostEl <- MaybeT $ H.getHTMLElementRef _ghost
    textWidth
      <- H.lift <<< H.liftEffect
      $ map _.width
      $ WHE.getBoundingClientRect
      $ ghostEl
    H.lift $ modifyState_ _ { width = textWidth }

focusInput :: forall pa cs m. MonadEffect m => ComponentM pa cs m Unit
focusInput =
  void $ runMaybeT do
    el <- MaybeT $ H.getHTMLElementRef _input
    H.lift $ H.liftEffect $ WHE.focus el

blurInput :: forall pa cs m. MonadEffect m => ComponentM pa cs m Unit
blurInput =
  void $ runMaybeT do
    el <- MaybeT $ H.getHTMLElementRef _input
    H.lift $ H.liftEffect $ WHE.blur el

-- | Action > Edit

type Edit a = ( cellEdit :: Unit | a )
_edit = SProxy :: SProxy "cellEdit"
edit :: forall a. V.Variant (Edit a)
edit = V.inj _edit unit

handleActionEdit :: forall pa cs m. MonadEffect m => Unit -> ComponentM pa cs m Unit
handleActionEdit _ = do
  modifyState_ \st -> st { editing = true, cache = st.value }
  focusInput
  updateInputWidth

-- | Action > OnValueInput

type OnValueInput a = ( cellOnValueInput :: String | a )
_onValueInput = SProxy :: SProxy "cellOnValueInput"
onValueInput :: forall a. String -> V.Variant (OnValueInput a)
onValueInput = V.inj _onValueInput

handleActionOnValueInput :: forall pa cs m. MonadEffect m => String -> ComponentM pa cs m Unit
handleActionOnValueInput str = do
  modifyState_ _ { cache = str }
  updateInputWidth

-- | Action > OnKeyDown

type OnKeyDown a = ( cellOnKeyDown :: KE.KeyboardEvent | a )
_onKeyDown = SProxy :: SProxy "cellOnKeyDown"
onKeyDown :: forall a. KE.KeyboardEvent -> V.Variant (OnKeyDown a)
onKeyDown = V.inj _onKeyDown

handleActionOnKeyDown :: forall pa cs m. MonadEffect m => KE.KeyboardEvent -> ComponentM pa cs m Unit
handleActionOnKeyDown ke = do
  case KE.key ke of
    "Enter" -> do
      H.liftEffect $ WE.preventDefault $ KE.toEvent ke
      modifyState_ \st -> st { editing = false, value = st.cache }
      blurInput

    "Escape" -> do
      H.liftEffect $ WE.preventDefault $ KE.toEvent ke
      modifyState_ \st -> st { editing = false }
      blurInput

    _ -> do
      pure unit

-- | Action > OnBlur

type OnBlur a = ( cellOnBlur :: Unit | a )
_onBlur = SProxy :: SProxy "cellOnBlur"
onBlur :: forall a. V.Variant (OnBlur a)
onBlur = V.inj _onBlur unit

handleActionOnBlur :: forall pa cs m. Unit -> ComponentM pa cs m Unit
handleActionOnBlur _ = do
  modifyState_ \st -> st { editing = false }

--------
-- Query

data Query x
  = GetValue (String -> x)

handleQuery :: forall pa cs m x. Query x -> ComponentM pa cs m (Maybe x)
handleQuery = case _ of
  GetValue reply -> do
    value <- getsState _.value
    pure $ Just <<< reply $ value

-- type QueryRow q
--   = QueryGetValue
--   + q

-- type Query q = VF.VariantF (QueryRow q)

-- handleQuery :: forall a q m x. MonadEffect m => Query q x -> ComponentM a m (Maybe x)
-- handleQuery
--   = VF.on _QueryGetValue handleQueryGetValue
--   $ VF.default (pure Nothing) -- TODO

-- | Query > QuerynUnhandled

-- TODO query all components to see if anyone can handle
-- type QueryUnhandled v q = ( _queryUnhandled :: FProxy ( Compose Maybe ((->) (V.Variant v)) ) | q )
-- _queryUnhandled = SProxy :: SProxy "_queryUnhandled"
-- queryUnhandled :: forall v q x. (Maybe (V.Variant v -> x)) -> VF.VariantF (QueryUnhandled v q) x
-- queryUnhandled = VF.inj _queryUnhandled <<< wrap

-- | Query > QueryGetValue

-- type QueryGetValue q = ( cellGetValue :: FProxy ((->) String) | q )
-- _QueryGetValue = SProxy :: SProxy "cellGetValue"
-- cellGetValue :: forall q a. (String -> a) -> VF.VariantF (QueryGetValue q) a
-- cellGetValue = VF.inj _QueryGetValue

-- handleQueryGetValue :: forall a m x. (String -> x) -> ComponentM a m (Maybe x)
-- handleQueryGetValue reply = do
--   value <- H.gets _.value
--   pure $ Just <<< reply $ value

--------
-- Input

type Input pa cs m =
  { value :: String
  , render :: ComponentRender pa cs m
  }

---------
-- Output

data Output a
  = RaiseUnhandled (V.Variant a)

--------
-- Types

type ComponentM pa cs m x = H.HalogenM (StateStore pa cs m) (Action pa) cs (Output pa) m x
type Component pa cs m = H.Component HH.HTML Query (Input pa cs m) (Output pa) m
type ComponentHTML pa cs m = H.ComponentHTML (Action pa) cs m
type ComponentRender pa cs m = State -> ComponentHTML pa cs m

type Slot pa = H.Slot Query (Output pa)

------------
-- Component

component :: forall pa cs m. MonadEffect m => Component pa cs m
component = H.mkComponent
  { initialState: \({value, render}) ->
      store render (defaultInitialState { value = value })
  , render: extract
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , handleQuery = handleQuery
    }
  }

----------
-- Setters

_input = H.RefLabel "input" :: H.RefLabel
_ghost = H.RefLabel "auto-size-input-ghost" :: H.RefLabel

-- inputProps :: forall t344 t345.
--   Array
--     (HH.IProp
--        ( onBlur :: FE.FocusEvent
--        , onFocus :: FE.FocusEvent
--        , onInput :: WE.Event
--        , onKeyDown :: KE.KeyboardEvent
--        , style :: String
--        , value :: String
--        | t344
--        )
--        (V.Variant
--           ( _edit :: Unit
--           , _onBlur :: Unit
--           , _onKeyDown :: KE.KeyboardEvent
--           , _onValueInput :: String
--           , _NSelectOnFocusInput :: Unit
--           , _NSelectOnKeyDownInput :: KE.KeyboardEvent
--           , _NSelectOnValueInput :: String
--           | t345
--           )
--        )
--     )
-- inputProps = setNSelectInputProps <<< setInputProps { cache: "", width: 0.0 } $ []

