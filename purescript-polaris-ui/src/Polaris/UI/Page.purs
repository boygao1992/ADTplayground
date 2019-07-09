module Polaris.UI.Page where

import Prelude

import Data.Const (Const)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Polaris.UI.Component.DataTable.Product (Slot, component) as DataTable
import Type.Data.Symbol (SProxy(..))
import Web.HTML as HTML
import Web.HTML.Window as Window
import Web.HTML.Location as Location

import Polaris.Data.Product (Product)
import Polaris.UI.Api.Product (getProducts) as Api
import Polaris.UI.Api.Login (login) as Api

type State =
  { hostname :: String
  , tableData :: Maybe (Array Product)
  , address :: String
  }

defaultInitialState :: State
defaultInitialState =
  { hostname: ""
  , tableData: Nothing
  , address: ""
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
render { tableData: mDataTableData, address } =
  HH.div_
  [ HH.text address
  , case mDataTableData of
      Nothing -> HH.text ""
      Just tableData ->
        HH.slot _table unit DataTable.component tableData (const Nothing)
  ]
handleAction :: forall m. MonadAff m => Action -> ComponentM m Unit
handleAction = case _ of
  Initialize -> do
    address <- H.liftEffect do
      window <- HTML.window
      location <- Window.location window
      Location.href location
    H.modify_ _ { address = address }

    hostname :: String
      <- H.gets _.hostname

    loginRes :: Maybe String
      <- H.liftAff
      <<< map (join <<< hush <<< _.body)
      <<< Api.login
      $ hostname

    case loginRes of
      Nothing -> do
        res :: Maybe (Array Product)
          <- H.liftAff
          <<< map (map (_.products <<< unwrap) <<< hush)
          <<< map _.body
          <<< Api.getProducts
          $ hostname
        H.modify_ _ { tableData = res }
      Just redirect_url -> H.liftEffect do
        window <- HTML.window
        location <- Window.location window
        Location.replace redirect_url location

