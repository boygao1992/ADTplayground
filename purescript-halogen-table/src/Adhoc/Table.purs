module Adhoc.Table where


import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
-- import Renderless.State (Store, extract, getState, getsState, modifyState_, modifyStore_, store)
import Type.Data.Symbol (SProxy(..))

import Adhoc.Row as Row
import Adhoc.Cell as Cell

type State = Array (Array (String /\ Cell.CellType))

-- type StateStore m = Store State (ComponentHTML m)

defaultInitialState :: State
defaultInitialState = []

data Action
  = NoOp

data Query a
  = GetValue (Map Int (Map String Cell.CellType) -> a)

type Input = State

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

render :: forall m. MonadAff m => ComponentRender m
render st =
  HH.div_ $
    Array.mapWithIndex renderRow st

  where
    renderRow :: Int -> Array (String /\ Cell.CellType) -> ComponentHTML m
    renderRow idx value =
      HH.slot _row idx Row.component value (const Nothing)

handleQuery :: forall m a. MonadAff m => Query a -> ComponentM m (Maybe a)
handleQuery = case _ of
  GetValue reply -> do
    rows <- H.queryAll _row (H.request $ Row.GetValue)
    pure $ Just $ reply rows
