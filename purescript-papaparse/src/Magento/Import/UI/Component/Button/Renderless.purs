module Magento.Import.UI.Component.Button.Renderless where

import Prelude

import Renderless.State (Store, extract, modifyStore_, store)
import Data.Const (Const)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH

type State = Unit

initialState :: State
initialState = unit

type StateStore m
  = Store State (ComponentHTML m)

data Action m
  = OnClick
  | Receive (Input m)

type Query = Const Void

type Input m = { render :: ComponentRender m }

data Output
  = Pressed

type ChildSlots = ()

type ComponentM m a = H.HalogenM (StateStore m) (Action m) ChildSlots Output m a
type Component m = H.Component HH.HTML Query (Input m) Output m
type ComponentHTML m = H.ComponentHTML (Action m) ChildSlots m
type ComponentRender m = State -> ComponentHTML m

type Slot = H.Slot Query Output

component :: forall m. MonadAff m => Component m
component = H.mkComponent
  { initialState: \({ render }) -> store render initialState
  , render: extract
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      }
  }

handleAction :: forall m. MonadAff m => Action m -> ComponentM m Unit
handleAction = case _ of
  OnClick -> do
    H.raise Pressed

  Receive { render } -> do
    modifyStore_ render identity

