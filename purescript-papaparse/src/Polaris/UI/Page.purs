module Polaris.UI.Page where


import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Either (hush)
import Data.Newtype (unwrap)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Polaris.UI.Component.DataTable.Product (Slot, component) as DataTable
import Type.Data.Symbol (SProxy(..))

import Polaris.Data.Product (Product)
import Polaris.UI.Api.Product (getProducts)

type State =
  { hostname :: String
  , tableData :: Maybe (Array Product)
  }

defaultInitialState :: State
defaultInitialState =
  { hostname: ""
  , tableData: Nothing
  }

data Action
  = Initialize

type Query = Const Void

type Input = String

type Output = Void

type ChildSlots =
  ( table :: DataTable.Slot Unit
  )
_table = SProxy :: SProxy "table"

type ComponentM m a = H.HalogenM State Action ChildSlots Output m a
type Component m = H.Component HH.HTML Query Input Output m
type ComponentHTML m = H.ComponentHTML Action ChildSlots m
type ComponentRender m = State -> ComponentHTML m

type Slot = H.Slot Query Output

component :: forall m. MonadAff m => Component m
component = H.mkComponent
  { initialState: \hostname -> defaultInitialState { hostname = hostname }
  , render
  , eval: H.mkEval $ H.defaultEval
      { initialize = Just Initialize
      , handleAction = handleAction
      }
  }

render :: forall m. MonadAff m => ComponentRender m
render { tableData: mDataTableData } =
  case mDataTableData of
    Nothing -> HH.text ""
    Just tableData ->
      HH.slot _table unit DataTable.component tableData (const Nothing)

handleAction :: forall m. MonadAff m => Action -> ComponentM m Unit
handleAction = case _ of
  Initialize -> do
    hostname :: String
      <- H.gets _.hostname
    res :: Maybe (Array Product)
      <- H.liftAff
      <<< map (map (_.products <<< unwrap) <<< hush)
      <<< map _.body
      <<< getProducts
      $ hostname
    H.modify_ _ { tableData = res }
