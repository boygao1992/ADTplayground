module Polaris.UI.Router where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Data.Symbol (SProxy(..))

import Polaris.UI.Data.Route (Route(..))
import Polaris.UI.Page.Home as Home
import Polaris.UI.Page.Svg as Svg
import Polaris.UI.Page.Table as Table
import Polaris.UI.Page.Config as Config
import Polaris.UI.Capability.Navigate (class Navigate, navigate)

import Polaris.UI.Block.Tabs as Tabs

type State =
  { route :: Route
  }

defaultInitialState :: State
defaultInitialState =
  { route: Home
  }

data Action
  = RouteTo Route

data Query a
  = Navigate Route a

type Input = Unit

type Output = Void

type ChildSlots =
  ( home :: Home.Slot Unit
  , svg :: Svg.Slot Unit
  , table :: Table.Slot Unit
  , config :: Table.Slot Unit
  )
_home = SProxy :: SProxy "home"
_svg = SProxy :: SProxy "svg"
_table = SProxy :: SProxy "table"
_config = SProxy :: SProxy "config"

type ComponentM m a = H.HalogenM State Action ChildSlots Output m a
type Component m = H.Component HH.HTML Query Input Output m
type ComponentHTML m = H.ComponentHTML Action ChildSlots m
type ComponentRender m = State -> ComponentHTML m

type Slot = H.Slot Query Output

component :: forall m. MonadAff m => Navigate m => Component m
component = H.mkComponent
  { initialState: const defaultInitialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      }
  }

render :: forall m. MonadAff m => ComponentRender m
render { route } =
  HH.div_
  [ Tabs.list_
    ( renderTabItem route <$>
      [ Home
      , Svg
      , Table
      , Config
      ]
    )
  , case route of
       Home -> HH.slot _home unit Home.component unit (const Nothing)
       Svg -> HH.slot _svg unit Svg.component unit (const Nothing)
       Table -> HH.slot _table unit Table.component unit (const Nothing)
       Config -> HH.slot _config unit Config.component unit (const Nothing)
  ]

  where
    renderTabItem currentRoute targetRoute =
      Tabs.container_
      [ Tabs.item (currentRoute == targetRoute)
        [ HE.onClick $ Just <<< const (RouteTo targetRoute)]
        [ Tabs.title_
          [ HH.text $ show targetRoute
          ]
        ]
      ]

handleAction :: forall m. Navigate m => Action -> ComponentM m Unit
handleAction = case _ of
  RouteTo route -> do
    navigate route

handleQuery :: forall m a. Query a -> ComponentM m (Maybe a)
handleQuery = case _ of
  Navigate route a -> Just a <$ do
    H.modify_ _ { route = route }
