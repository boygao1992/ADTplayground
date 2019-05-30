module Magento.Import.UI.Page where

import Prelude

import Data.Tuple (Tuple(..))
import Data.String.Read (read)
import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
-- import Halogen.HTML.Properties as HP
-- import Halogen.Query.EventSource as HES
import Halogen.Util (debugShow)
import Type.Data.Symbol (SProxy(..))
import Magento.Import.UI.Container.Categories as Categories
import Magento.Import.Data.Skus (Sku(..))
import Ocelot.Block.Button (button) as Button

type State = Unit

initialState :: State
initialState = unit

data Action
  = GetAllValidCategories

type Query = Const Void

type Input = Unit

type Output = Void

type ChildSlots =
  ( categories :: Categories.Slot Unit
  )

_categories = SProxy :: SProxy "categories"

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
  [ HH.slot _categories unit Categories.component
      [ Tuple (Sku "FSWT400")
        [ { category: fromMaybe mempty $ read "Finestra Wood Category/Traverse/Components/Cord Draw"
          , validity: false
          }
          , { category: fromMaybe mempty $ read "Finestra Wood Category/Traverse/Components/Baton Draw"
          , validity: true
          }
        ]
        , Tuple (Sku "CP500")
        [ { category: fromMaybe mempty $ read "Rowley Category/Drapery Hardware/Tracks & Components/Cord Draw Track"
          , validity: true
          }
        , { category: fromMaybe mempty $ read "A/B/C"
          , validity: false
          }
        ]
      ]
      (const Nothing)
  , Button.button
    [ HE.onClick $ Just <<< const GetAllValidCategories ]
    [ HH.text "getAllValidCategories" ]
  ]

handleAction :: forall m. MonadAff m => Action -> ComponentM m Unit
handleAction = case _ of
  GetAllValidCategories -> do
    result <- H.query _categories unit (H.request Categories.GetAllCategories)
    debugShow result


