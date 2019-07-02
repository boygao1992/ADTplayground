module Polaris.UI.Component.Table where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Polaris.UI.Block.DataTable as DataTable
import Polaris.UI.Component.Table.Row as Row
import Type.Data.Symbol (SProxy(..))

type State =
  { value :: Array ( Array (Tuple String String))
  }

defaultInitialState :: State
defaultInitialState =
  { value: []
  }

data Action
  = NoOp

data Query a
  = GetValue (Map.Map Int (Map.Map String String) -> a)

type Input = Array (Array (Tuple String String))

type Output = Void

type ChildSlots =
  ( row :: Row.Slot Int
  )

_row = SProxy :: SProxy "row"

type ComponentM m a = H.HalogenM State Action ChildSlots Output m a
type Component m = H.Component HH.HTML Query Input Output m
type ComponentHTML m = H.ComponentHTML Action ChildSlots m
type ComponentRender m = State -> ComponentHTML m

type Slot = H.Slot Query Output

component :: forall m. MonadAff m => Component m
component = H.mkComponent
  { initialState: \value ->
      defaultInitialState
        { value = value
        }
  , render
  , eval: H.mkEval H.defaultEval
      { handleQuery = handleQuery
      }
  }

render :: forall m. MonadAff m => ComponentRender m
render { value } =
  DataTable.table_
  $ Array.mapWithIndex renderRow value
  where
    renderRow :: Int -> Array (Tuple String String) -> ComponentHTML m
    renderRow idx rowValue =
      HH.slot _row idx Row.component rowValue (const Nothing)

handleQuery :: forall m a. MonadAff m => Query a -> ComponentM m (Maybe a)
handleQuery = case _ of
  GetValue reply ->
    pure <<< Just <<< reply <=< H.queryAll _row $ H.request Row.GetValue
