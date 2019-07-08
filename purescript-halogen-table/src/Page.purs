module Page where

import Prelude

import Adhoc.Adapter as Adapter
import Adhoc.Cell as Cell
import Adhoc.Table as Table
import Data.Const (Const)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Type.Data.Symbol (SProxy(..))

data Position
  = Boss
  | Contractor
derive instance genericPosition :: Generic Position _
instance showPosition :: Show Position where show = genericShow

data Country
  = None
  | Germancy
  | Greece
  | Finland
  | Poland
  | Sweden
derive instance genericCountry :: Generic Country _
instance showCountry :: Show Country where show = genericShow

type People =
  { name :: String
  , position :: Position
  , country :: Country
  , salary :: Number
  , active :: Boolean
  }

sampleData :: Array People
sampleData =
  [ { name: "Arthur Brown", position: Contractor, country: None, salary: 8877.0, active: false }
  , { name: "Ike Hill", position: Boss, country: Sweden, salary: 34835.0, active: true }
  ]

type State = Unit
type Action = Unit
type Query = Const Void
type Input = Unit
type Output = Void
type ChildSlots =
  ( cell :: Cell.Slot Int
  , table :: Table.Slot Unit
  )
_cell = SProxy :: SProxy "cell"
_table = SProxy :: SProxy "table"

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
  HH.div_
  [ HH.slot _cell 0 Cell.component
    { render: Cell.defaultRender
    , value: Cell.CellTypeInt 123
    }
    (const Nothing)
  , HH.slot _cell 1 Cell.component
    { render: Cell.defaultRender
    , value: Cell.CellTypeNumber 1234.56789
    }
    (const Nothing)
  , HH.slot _cell 2 Cell.component
    { render: Cell.defaultRender
    , value: Cell.CellTypeFixed 3 12345.678
    }
    (const Nothing)
  , HH.slot _cell 3 Cell.component
    { render: Cell.defaultRender
    , value: Cell.CellTypeString "hello world"
    }
    (const Nothing)
  , HH.slot _cell 4 Cell.component
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
  , HH.slot _cell 5 Cell.component
      { render: Cell.defaultRender
      , value: Cell.CellTypeBoolean false
      }
      (const Nothing)
  , HH.slot _table unit Table.component
      (Adapter.rowInputAdapter <$> sampleData)
      (const Nothing)
  ]

