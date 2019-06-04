module Magento.Import.UI.Container.Categories where

import Prelude

import Data.Foldable (foldMap)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Magento.Import.Data.Categories (Categories, Category)
import Magento.Import.Data.Skus (Sku)
import Magento.Import.UI.Container.Categories.Category as Category
import Magento.Import.UI.Data.Validity as Validity
import Ocelot.Block.Card as Card
import Ocelot.Block.Expandable as Expandable
import Type.Data.Symbol (SProxy(..))

type State =
  { initCategories ::
      Array
      ( Tuple Sku
          (Array { category :: Category, validity :: Boolean })
      )
  , expand :: Expandable.Status
  , validity :: Maybe Validity.Validity
  , visibility :: Boolean
  }

defaultInitialState :: State
defaultInitialState =
  { initCategories: mempty
  , expand: Expandable.Expanded -- TODO for styling
  , validity: Nothing
  , visibility: false
  }

data Action
  = ToggleExpand
  | HandleCategory Sku Category.Output
  | Initialize
  | Receive Input

data Query a
  = GetAllCategories (Map Sku Categories -> a)

type Input =
  { initCategories ::
    Array
    ( Tuple Sku
      (Array { category :: Category, validity :: Boolean })
    )
  , visibility :: Boolean
  }


type Output = Void

type ChildSlots =
  ( category :: Category.Slot Sku
  )

_category = SProxy :: SProxy "category"

type ComponentM m a = H.HalogenM State Action ChildSlots Output m a
type Component m = H.Component HH.HTML Query Input Output m
type ComponentHTML m = H.ComponentHTML Action ChildSlots m
type ComponentRender m = State -> ComponentHTML m

type Slot = H.Slot Query Output


component :: forall m. MonadAff m => Component m
component = H.mkComponent
  { initialState: \({initCategories, visibility}) ->
      defaultInitialState
      { initCategories = initCategories
      , visibility = visibility
      }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , initialize = Just Initialize
      , receive = Just <<< Receive
      }
  }

render :: forall m. MonadAff m => ComponentRender m
render { initCategories, expand, validity, visibility } =
  Card.card
  [ HP.classes $ HH.ClassName <$>
    [ Validity.backgroundColorLightest validity
    , if visibility then "" else "hidden"
    ]
  ]
  [ Expandable.heading
    [ HE.onClick $ Just <<< const ToggleExpand]
    [ HH.text "Categories"]
  , Expandable.content_
    expand
    (map renderCategory initCategories)
  ]
  where
    renderCategory :: (Tuple Sku (Array { category :: Category, validity :: Boolean })) -> ComponentHTML m
    renderCategory (Tuple sku categories) =
      HH.slot _category sku Category.component
        { sku, categories } (Just <<< HandleCategory sku)

handleAction :: forall m. MonadAff m => Action -> ComponentM m Unit
handleAction = case _ of
  ToggleExpand -> do
    H.modify_ \st -> st { expand = not st.expand }

  HandleCategory sku Category.StatusUpdate -> do
    vs <- H.queryAll _category (H.request Category.GetValidity)
    H.modify_ _ { validity = foldMap identity vs }

  Initialize -> do
    vs <- H.queryAll _category (H.request Category.GetValidity)
    H.modify_ _ { validity = foldMap identity vs }

  Receive { visibility } -> do
    H.modify_ _ { visibility = visibility }


handleQuery :: forall m a. MonadAff m => Query a -> ComponentM m (Maybe a)
handleQuery = case _ of
  GetAllCategories reply -> do
    result <- H.queryAll _category (H.request Category.GetCategories)
    pure $ Just $ reply result

