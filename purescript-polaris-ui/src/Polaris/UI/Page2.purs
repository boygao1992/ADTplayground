module Polaris.UI.Page2 where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Polaris.UI.Component.Table (Slot, component) as Table
import Type.Data.Symbol (SProxy(..))

type State =
  { tableData :: Maybe (Array (Array (Tuple String String)))
  }

defaultInitialState :: State
defaultInitialState =
  { tableData: Just
    [ [ Tuple "Product" "Emerald Silk Gown"
      , Tuple "Price" "$875.00"
      , Tuple "SKU Number" "124689"
      , Tuple "Net quantity" "140"
      , Tuple "Net sales" "$122,500.00"
      ]
    , [ Tuple "Product" "Emerald Silk Gown"
      , Tuple "Price" "$875.00"
      , Tuple "SKU Number" "124689"
      , Tuple "Net quantity" "140"
      , Tuple "Net sales" "$122,500.00"
      ]
    , [ Tuple "Product" "Emerald Silk Gown"
      , Tuple "Price" "$875.00"
      , Tuple "SKU Number" "124689"
      , Tuple "Net quantity" "140"
      , Tuple "Net sales" "$122,500.00"
      ]
    , [ Tuple "Product" "Emerald Silk Gown"
      , Tuple "Price" "$875.00"
      , Tuple "SKU Number" "124689"
      , Tuple "Net quantity" "140"
      , Tuple "Net sales" "$122,500.00"
      ]
    ]
  }

data Action
  = NoOp

type Query = Const Void

type Input = Unit

type Output = Void

type ChildSlots =
  ( table :: Table.Slot Unit
  )
_table = SProxy :: SProxy "table"

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
  }

render :: forall m. MonadAff m => ComponentRender m
render { tableData: mDataTableData } =
  case mDataTableData of
    Nothing -> HH.text ""
    Just tableData ->
      HH.slot _table unit Table.component tableData (const Nothing)
