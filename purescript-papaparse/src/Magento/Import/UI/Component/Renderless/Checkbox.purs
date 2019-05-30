module Magento.Import.UI.Component.Renderless.Checkbox where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Const (Const)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Renderless.State (Store, extract, getState, modifyStore_, putState, store)

type State = Boolean

defaultInitialState :: State
defaultInitialState = false

type StateStore m = Store State (ComponentHTML m)

data Action m
  = Toggle
  | Receive (Input m)

type Query = Const Void

type Input m =
  { render :: ComponentRender m
  , initialState :: State
  }

data Output
  = Checked Boolean

type ChildSlots = ()

type ComponentM m a = H.HalogenM (StateStore m) (Action m) ChildSlots Output m a
type Component m = H.Component HH.HTML Query (Input m) Output m
type ComponentHTML m = H.ComponentHTML (Action m) ChildSlots m
type ComponentRender m = State -> ComponentHTML m

type Slot = H.Slot Query Output

component :: forall m. MonadAff m => Component m
component = H.mkComponent
  { initialState: \({ render, initialState }) -> store render initialState
  , render: extract
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }

handleAction :: forall m. MonadAff m => Action m -> ComponentM m Unit
handleAction = case _ of
  Toggle -> do
    st <- getState
    let checked = not st
    putState checked
    H.raise $ Checked checked

  Receive { render } -> do
    modifyStore_ render identity

