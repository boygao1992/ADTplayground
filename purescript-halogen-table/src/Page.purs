module Page where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Effect.Aff.Class (class MonadAff)
import Type.Data.Symbol (SProxy(..))
import Adhoc.Cell as Cell

type State = Unit
type Action = Unit
type Query = Const Void
type Input = Unit
type Output = Void
type ChildSlots =
  ( cell :: Cell.Slot Unit
  )
_cell = SProxy :: SProxy "cell"

type ComponentM m a = H.HalogenM State Action ChildSlots Output m a
type Component m = H.Component HH.HTML Query Input Output m
type ComponentHTML m = H.ComponentHTML Action ChildSlots m
type ComponentRender m = State -> ComponentHTML m

type Slot = H.Slot Query Output

component :: forall m. MonadAff m => Component m
component = H.mkComponent
  { initialState: const unit
  , render
  , eval: H.mkEval H.defaultEval
  }

render :: forall m. MonadAff m => ComponentRender m
render _ =
  HH.slot _cell unit Cell.component
    { render: Cell.defaultRender
    , value: Cell.CellTypeEnum
      [ "purescript-css-validate"
      , "purescript-halogen-color-picker"
      , "purescript-halogen-day-picker"
      , "purescript-halogen-nselect"
      , "purescript-halogen-storybook"
      , "purescript-halogen-transition"
      , "purescript-jest"
      , "purescript-svg-parser"
      , "purescript-svgo"
      , "svgen"
      ]
      "purescript-svgo"
    }
    (const Nothing)

