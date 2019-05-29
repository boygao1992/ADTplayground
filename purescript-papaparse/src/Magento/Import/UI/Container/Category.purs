module Magento.Import.UI.Container.Category where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
-- import Halogen.HTML.Events as HE
-- import Halogen.HTML.Properties as HP
-- import Halogen.Query.EventSource as HES
-- import Halogen.Util (debug, debugShow)
import Magento.Import.Data.Categories (Categories(..), Category)
import Magento.Import.UI.Container.Category.Item as Item
import Type.Data.Symbol (SProxy(..))
import Data.Array as Array

type State =
  { categories :: Array { category :: Category, validity :: Boolean }
  }

defaultInitialState :: State
defaultInitialState =
  { categories: mempty
  }

data Action
  = HandleItem Int Item.Output

data Query a
  = GetCategories (Categories -> a)

type Input =
  { categories :: Array { category :: Category, validity :: Boolean }
  }

type Output = Void

type ChildSlots =
  ( item :: Item.Slot Int
  )

_item = SProxy :: SProxy "item"

type ComponentM m a = H.HalogenM State Action ChildSlots Output m a
type Component m = H.Component HH.HTML Query Input Output m
type ComponentHTML m = H.ComponentHTML Action ChildSlots m
type ComponentRender m = State -> ComponentHTML m

type Slot = H.Slot Query Output
component :: forall m. MonadAff m => Component m
component = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleQuery = handleQuery
      }
  }

render :: forall m. MonadAff m => ComponentRender m
render { categories } =
  HH.div_
  (renderItems categories)
  where
    renderItems
      :: Array { category :: Category, validity :: Boolean }
      -> Array (ComponentHTML m)
    renderItems = Array.mapWithIndex \idx i ->
      HH.slot _item idx Item.component i (const Nothing)

handleQuery :: forall m a. MonadAff m => Query a -> ComponentM m (Maybe a)
handleQuery = case _ of
  GetCategories reply -> do
    result <- H.queryAll _item (H.request Item.GetCategory)
    pure $ Just $ reply $ Categories <<< Array.fromFoldable $ result
