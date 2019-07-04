module Page where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Const (Const)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Type.Data.Symbol (SProxy(..))

import Ocelot.Component.DatePicker as DatePicker

type Component m = H.Component HH.HTML Query Input Output m
type ComponentHTML m = H.ComponentHTML Action ChildSlots m
type ComponentRender m = State -> ComponentHTML m
type ComponentM m a = H.HalogenM State Action ChildSlots Output m a

type State = Unit
type Input = Unit
type Output = Void
data Action = NoOp
type Query = Const Void
type ChildSlots =
  ( datepicker :: DatePicker.Slot Unit )
_datepicker = SProxy :: SProxy "datepicker"

component :: forall m. MonadAff m => Component m
component = H.mkComponent
  { initialState: const unit
  , render
  , eval: H.mkEval H.defaultEval
  }

render :: forall m. MonadAff m => ComponentRender m
render _ =
  HH.slot _datepicker unit DatePicker.component
    { targetDate: Nothing
    , selection: Nothing
    }
    (const Nothing)

