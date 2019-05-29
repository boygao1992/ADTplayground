module Magento.Import.UI.Page where

import Prelude

import Data.String.Read (read)
import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
-- import Halogen.HTML.Properties as HP
-- import Halogen.Query.EventSource as HES
import Halogen.Util (debug, debugShow)
import Type.Data.Symbol (SProxy(..))
import Magento.Import.UI.Container.Category as Category

type State = Unit

initialState :: State
initialState = unit

data Action
  = GetAllValidCategories

type Query = Const Void

type Input = Unit

type Output = Void

type ChildSlots =
  ( category :: Category.Slot Unit
  )

_category = SProxy :: SProxy "category"

type ComponentM m a = H.HalogenM State Action ChildSlots Output m a
type Component m = H.Component HH.HTML Query Input Output m
type ComponentHTML m = H.ComponentHTML Action ChildSlots m
type ComponentRender m = State -> ComponentHTML m

component :: forall m. MonadAff m => Component m
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      }
  }

render :: forall m. MonadAff m => State -> ComponentHTML m
render _ =
  HH.div_
  [ HH.slot _category unit Category.component
      { categories:
        [ { category: fromMaybe mempty (read "A/B")
          , validity: false
          }
        , { category: fromMaybe mempty (read "A/C/D/E")
          , validity: true
          }
        , { category: fromMaybe mempty (read "A/F/G")
          , validity: false
          }
        ]
      }
      (const Nothing)
  , HH.button
    [ HE.onClick $ Just <<< const GetAllValidCategories ]
    [ HH.text "getAllCategories" ]
  ]

handleAction :: forall m. MonadAff m => Action -> ComponentM m Unit
handleAction = case _ of
  GetAllValidCategories -> do
    result <- H.query _category unit (H.request Category.GetCategories)
    debugShow result


