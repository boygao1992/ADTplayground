module Adhoc.Row where


import Prelude

import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
-- import Renderless.State (Store, extract, getState, getsState, modifyState_, modifyStore_, store)
import Type.Data.Symbol (SProxy(..))

import Adhoc.Cell as Cell

type State = Array (String /\ Cell.CellType)

-- type StateStore m = Store State (ComponentHTML m)

defaultInitialState :: State
defaultInitialState = []

data Action
  = NoOp

data Query a
  = GetValue (Map String Cell.CellType -> a)

type Input = State

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
  { initialState: identity
  , render
  , eval: H.mkEval H.defaultEval
      { handleQuery = handleQuery
      }
  }

render :: forall m. MonadAff m => ComponentRender m
render st =
  HH.div_ $
    uncurry renderCell <$> st

  where
    renderCell :: String -> Cell.CellType -> ComponentHTML m
    renderCell label value =
      HH.slot _cell label Cell.component
        { render: Cell.defaultRender
        , value
        }
        (const Nothing)

handleQuery :: forall m a. MonadAff m => Query a -> ComponentM m (Maybe a)
handleQuery = case _ of
  GetValue reply -> do
    cells <- H.queryAll _cell (H.request $ Cell.GetValue)
    pure $ Just $ reply cells
