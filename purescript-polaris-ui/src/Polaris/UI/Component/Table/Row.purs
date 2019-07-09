module Polaris.UI.Component.Table.Row where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
-- import Halogen.HTML.Events as HE
-- import Halogen.HTML.Properties as HP
import Polaris.UI.Block.DataTable as DataTable
import Type.Data.Symbol (SProxy(..))

import Polaris.UI.Component.Table.Cell as Cell


type State =
  { value :: Array (Tuple String String)
  }

defaultInitialState :: State
defaultInitialState =
  { value: []
  }

data Action
  = NoOp

data Query a
  = GetValue (Map.Map String String -> a)

type Input = Array (Tuple String String)

type Output = Void

type ChildSlots =
  ( cell :: Cell.Slot String
  )

_cell = SProxy :: SProxy "cell"

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
  DataTable.row_
  $ renderCell <$> value
  where
    renderCell :: Tuple String String -> ComponentHTML m
    renderCell (Tuple columnName cellValue) =
      HH.slot _cell columnName Cell.component cellValue (const Nothing)

handleQuery :: forall m a. MonadAff m => Query a -> ComponentM m (Maybe a)
handleQuery = case _ of
  GetValue reply ->
    pure <<< Just <<< reply <=< H.queryAll _cell $ H.request Cell.GetValue
