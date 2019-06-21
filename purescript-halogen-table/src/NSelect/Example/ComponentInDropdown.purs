module NSelect.Example.ComponentInDropdown where

import Prelude

import Control.MonadPlus (guard)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import NSelect.Example.ComponentInDropdown.Child as Child
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import NSelect as S
import NSelect.Setters as SS

type State =
  { value :: String
  }

initialState :: State
initialState =
  { value: ""
  }

type Query = Const Void

data Action
  = HandleDropdown (S.Output Action)
  | HandleChild Child.Output

type Input = Unit

type Output = Void

type ChildSlots =
  ( dropdown :: S.Slot Action Unit
  )

type SelectSlots =
  ( child :: Child.Slot Unit
  )

_dropdown = SProxy :: SProxy "dropdown"
_child = SProxy :: SProxy "child"

type ComponentM m a = H.HalogenM State Action ChildSlots Output m a
type Component m = H.Component HH.HTML Query Input Output m
type ComponentHTML m = H.ComponentHTML Action ChildSlots m
type ComponentRender m = State -> ComponentHTML m

type Slot = H.Slot Query Output

component :: forall m. MonadAff m => Component m
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction }
  }

render :: forall m. MonadAff m => State -> ComponentHTML m
render state =
  HH.div_
  [ HH.p [ HP.class_ $ HH.ClassName "mb-3"]
    [ HH.text "Render another component inside dropdown."
    ]
  , HH.slot _dropdown unit S.component
      { render: renderSelect
      , itemCount: 0
      }
      $ Just <<< HandleDropdown
  , HH.div_
    [ HH.text $ "You typed: " <> state.value
    ]
  ]
  where
    renderSelect :: S.ComponentRender Action SelectSlots m
    renderSelect st =
      HH.div  ( SS.setRootProps
                [ HP.class_ $ HH.ClassName "inline-block" ]
              )
      $ join
      [ pure $
        HH.button ( SS.setToggleProps [] )
        [ HH.text "toggle" ]
      , guard st.isOpen $>
        HH.div [ HP.class_ $ HH.ClassName "shadow-md absolute bg-white p-4" ]
        [ HH.slot _child unit Child.component unit
          (Just <<< S.raise <<< HandleChild)
        ]
      ]

handleAction :: forall m. MonadAff m => Action -> ComponentM m Unit
handleAction = case _ of
  HandleDropdown msg -> do
    case msg of
      S.Emit q -> handleAction q
      _ -> pure unit
  HandleChild value -> do
    H.modify_ $ _ { value = value }
