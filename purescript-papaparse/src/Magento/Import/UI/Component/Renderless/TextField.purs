module Magento.Import.UI.Component.Renderless.TextField where

import Prelude

import Renderless.State (Store, extract, putState, store)
import Data.Const (Const)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH

type State = String

defaultInitialState :: State
defaultInitialState = ""

type StateStore m = Store State (ComponentHTML m)

data Action
  = KeyboardInput String

type Query = Const Void

type Input m =
  { render :: ComponentRender m
  , initialState :: State
  }

data Output
  = Updated String

type ChildSlots = ()

type ComponentM m a = H.HalogenM (StateStore m) Action ChildSlots Output m a
type Component m = H.Component HH.HTML Query (Input m) Output m
type ComponentHTML m = H.ComponentHTML Action ChildSlots m
type ComponentRender m = State -> ComponentHTML m

type Slot = H.Slot Query Output

component :: forall m. MonadAff m => Component m
component = H.mkComponent
  { initialState: \({render, initialState}) -> store render initialState
  , render: extract
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      }
  }

handleAction :: forall m. MonadAff m => Action -> ComponentM m Unit
handleAction = case _ of
  KeyboardInput str -> do
    putState str
    H.raise $ Updated str

