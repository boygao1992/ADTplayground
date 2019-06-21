module Table.Cell.Renderless where

import Prelude

import Renderless.State (Store, extract, getsState, modifyState_, store)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Web.Event.Event as WE
import Web.HTML.HTMLElement as WHE
import Web.UIEvent.KeyboardEvent as KE

type State =
  { editing :: Boolean
  , value :: String
  , cache :: String
  , width :: Number
  }

type StateStore m = Store State (ComponentHTML m)

defaultInitialState :: State
defaultInitialState =
  { editing: false
  , value: ""
  , cache: ""
  , width: 0.0
  }

data Action
  = Edit
  | OnValueInput String
  | OnKeyDown KE.KeyboardEvent
  | OnBlur

data Query a
  = GetValue (String -> a)

type Input m =
  { render :: ComponentRender m
  , value :: String
  }

type Output = Void

type ChildSlots = ()

type ComponentM m a = H.HalogenM (StateStore m) Action ChildSlots Output m a
type Component m = H.Component HH.HTML Query (Input m) Output m
type ComponentHTML m = H.ComponentHTML Action ChildSlots m
type ComponentRender m = State -> ComponentHTML m

type Slot = H.Slot Query Output

component :: forall m. MonadAff m => Component m
component = H.mkComponent
  { initialState: \({ render, value}) -> store render defaultInitialState
      { value = value
      }
  , render: extract
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      }
  }

inputRef = H.RefLabel "input" :: H.RefLabel
ghostRef = H.RefLabel "auto-size-input-ghost" :: H.RefLabel

handleAction :: forall m. MonadAff m => Action -> ComponentM m Unit
handleAction = case _ of
  Edit -> do
    modifyState_ \st -> st { editing = true, cache = st.value }
    focusInput
    updateInputWidth

  OnValueInput str -> do
    modifyState_ _ { cache = str }
    updateInputWidth

  OnKeyDown ke -> do
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

  OnBlur -> do
    modifyState_ \st -> st { editing = false }

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

    blurInput :: ComponentM m Unit
    blurInput =
      void $ runMaybeT do
        el <- MaybeT $ H.getHTMLElementRef inputRef
        H.lift $ H.liftEffect $ WHE.blur el

handleQuery :: forall m a. MonadAff m => Query a -> ComponentM m (Maybe a)
handleQuery = case _ of
  GetValue reply -> do
    value <- getsState _.value
    pure $ Just <<< reply $ value

