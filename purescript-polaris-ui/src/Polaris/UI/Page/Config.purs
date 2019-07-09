module Polaris.UI.Page.Config where

import Prelude

import Data.Const (Const)
import Halogen as H
import Halogen.HTML as HH

type State = Unit

defaultInitialState :: State
defaultInitialState = unit

data Action
  = NoOp

type Query = Const Void

type Input = Unit

type Output = Void

type ChildSlots = ()

type ComponentM m a = H.HalogenM State Action ChildSlots Output m a
type Component m = H.Component HH.HTML Query Input Output m
type ComponentHTML m = H.ComponentHTML Action ChildSlots m
type ComponentRender m = State -> ComponentHTML m

type Slot = H.Slot Query Output

component :: forall m. Component m
component = H.mkComponent
  { initialState: const unit
  , render
  , eval: H.mkEval H.defaultEval
  }

render :: forall m. ComponentRender m
render _ =
  HH.div_
  [ HH.h1_
    [ HH.text "Config" ]
  ]
