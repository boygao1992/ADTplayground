module NSelect.Example.ComponentInDropdown.Child where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
  { value :: String
  }

defaultInitialState :: State
defaultInitialState =
  { value: ""
  }

data Action
  = OnInput String

type Query = Const Void

type Input = Unit

type Output = String

type ChildSlots = ()

type ComponentM m a = H.HalogenM State Action ChildSlots Output m a
type Component m = H.Component HH.HTML Query Input Output m
type ComponentHTML m = H.ComponentHTML Action ChildSlots m
type ComponentRender m = State -> ComponentHTML m

type Slot = H.Slot Query Output

component :: forall m. MonadAff m => Component m
component = H.mkComponent
  { initialState: const defaultInitialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction }
  }

render :: forall m. MonadAff m => ComponentRender m
render state =
  HH.div_
  [ HH.input
    [ HP.value state.value
    , HE.onValueInput $ Just <<< OnInput
    ]
  ]

handleAction :: forall m. MonadAff m => Action -> ComponentM m Unit
handleAction (OnInput value) = do
  H.modify_ _ { value = value }
  H.raise value
