module Polaris.UI.Page where


import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Either (either)
import Data.Newtype (unwrap)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log, logShow)
import Halogen as H
import Halogen.HTML as HH
import Polaris.UI.Component.Table.Product (Slot, component) as Table
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
  { initialState: \hostname -> defaultInitialState { hostname = hostname }
  , render
  , eval: H.mkEval $ H.defaultEval
      { initialize = Just Initialize
      , handleAction = handleAction
      }
  }

render :: forall m. MonadAff m => ComponentRender m
render { tableData: mTableData } =
  case mTableData of
    Nothing -> HH.text ""
    Just tableData ->
      HH.slot _table unit Table.component tableData (const Nothing)

handleAction :: forall m. MonadAff m => Action -> ComponentM m Unit
handleAction = case _ of
  Initialize -> do
    -- TODO debug
    H.liftEffect $ log "initialize"

    hostname <- H.gets _.hostname
    res
      <- H.liftAff $ map _.body <<< getProducts $ hostname

    -- TODO debug
    H.liftEffect $ logShow res

    H.modify_ _ { tableData = (map (_.products <<< unwrap) <<< either (const Nothing) Just) $ res }
    pure unit
