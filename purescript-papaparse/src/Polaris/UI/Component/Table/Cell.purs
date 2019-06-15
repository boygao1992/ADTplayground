module Polaris.UI.Component.Table.Cell where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.HTML.HTMLElement as WHE
import Web.Event.Event as WE
import Web.UIEvent.KeyboardEvent as KE
import Polaris.UI.Block.DataTable as DataTable
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)

type State =
  { editing :: Boolean
  , value :: String
  , cache :: String
  }

defaultInitialState :: State
defaultInitialState =
  { editing: false
  , value: "hello world"
  , cache: ""
  }

data Action
  = Edit
  | OnInput String
  | OnKeyEvent KE.KeyboardEvent
  | OnBlur

type Query = Const Void

type Input = Unit

type Output = Void

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
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      }
  }

_input :: H.RefLabel
_input = H.RefLabel "input"

render :: forall m. MonadAff m => ComponentRender m
render { editing, value, cache } =
  DataTable.cell_
  [ if editing
    then
      HH.input [ HP.value cache
               , HE.onKeyDown $ Just <<< OnKeyEvent
               , HE.onValueInput $ Just <<< OnInput
               , HE.onBlur $ Just <<< const OnBlur
               , HP.ref $ _input]
    else
      HH.p [ HE.onClick <<< const <<< Just $ Edit
           ]
      [ HH.text value ]
  ]

handleAction :: forall m. MonadAff m => Action -> ComponentM m Unit
handleAction = case _ of
  Edit -> do
    H.modify_ \st -> st { editing = true, cache = st.value }
    void $ runMaybeT do
      el <- MaybeT $ H.getHTMLElementRef _input
      H.lift $ H.liftEffect $ WHE.focus el

  OnInput str -> do
    H.modify_ _ { cache = str }

  OnKeyEvent ke -> do
    H.liftEffect $ log $ KE.key ke
    case KE.key ke of
      "Enter" -> do
        H.liftEffect $ WE.preventDefault $ KE.toEvent ke
        H.modify_ \st -> st { editing = false, value = st.cache }
        void $ runMaybeT do
          el <- MaybeT $ H.getHTMLElementRef _input
          H.lift $ H.liftEffect $ WHE.focus el
      "Escape" -> do
        H.liftEffect $ WE.preventDefault $ KE.toEvent ke
        H.modify_ \st -> st { editing = false }
        void $ runMaybeT do
          el <- MaybeT $ H.getHTMLElementRef _input
          H.lift $ H.liftEffect $ WHE.focus el
      _ -> do
        pure unit

  OnBlur -> do
    H.modify_ \st -> st { editing = false }


