module Magento.Import.UI.Container.Categories.Category where

import Prelude

import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Util (debug)
import Magento.Import.Data.Categories (Categories(..), Category)
import Magento.Import.Data.Skus (Sku)
import Magento.Import.UI.Container.Categories.Item as Item
import Magento.Import.UI.Data.Validity as Validity
import Ocelot.Block.Button (button) as Ocelot
import Ocelot.Block.Card as Card
import Ocelot.Block.Expandable as Expandable
import Type.Data.Symbol (SProxy(..))

type State =
  { categories :: Array { category :: Category, validity :: Validity.Validity, selected :: Boolean }
  , sku :: Sku
  , expand :: Expandable.Status
  , validity :: Maybe Validity.Validity
  }

defaultInitialState :: State
defaultInitialState =
  { categories: mempty
  , expand: Expandable.Expanded -- TODO for styling
  , sku: mempty
  , validity: Nothing
  }

data Action
  = AddCategory
  | ToggleExpand
  | HandleItem Int Item.Output
  | Initialize

data Query a
  = GetCategories (Categories -> a)
  | GetValidity (Maybe Validity.Validity -> a)

type Input =
  { categories :: Array { category :: Category, validity :: Boolean }
  , sku :: Sku
  }

data Output
  = StatusUpdate

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
  { initialState: \({categories, sku}) ->
      defaultInitialState
      { categories = (\({category, validity}) ->
                        { category
                        , validity: if validity then Validity.Valid else Validity.Invalid
                        , selected: true
                        }) <$> categories
      , sku = sku
      }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , initialize = Just Initialize
      }
  }

render :: forall m. MonadAff m => ComponentRender m
render { categories, expand, sku, validity } =
  Card.innerCard
  [ HP.classes $ HH.ClassName <$>
    [ Validity.backgroundColorLighter validity
    , "rounded"
    ]
  ]
  [ Expandable.heading
    [ HE.onClick
      $ Just <<< const ToggleExpand
    , Expandable.status expand
    ]
    [ HH.text $ show sku
    ]
  , Expandable.content_
    expand
    ( renderItems categories <>
      [ Card.innerCard_
        [ Ocelot.button [HE.onClick $ Just <<< const AddCategory]
          [ HH.text "+"]
        ]
      ]
    )
  ]
  where
    renderItems
      :: Array { category :: Category, validity :: Validity.Validity, selected :: Boolean }
      -> Array (ComponentHTML m)
    renderItems = Array.mapWithIndex \idx i ->
      HH.slot _item idx Item.component i (Just <<< HandleItem idx)

handleAction :: forall m. MonadAff m => Action -> ComponentM m Unit
handleAction = case _ of
  AddCategory -> do
    debug "add category"
    H.modify_ $ \(st@{categories}) ->
      st { categories = categories
              `Array.snoc` {category: mempty, validity: Validity.NotValidated, selected: false}
         }

  ToggleExpand -> do
    H.modify_ \st -> st { expand = not st.expand }

  HandleItem no (Item.StatusUpdate) -> do
    vs <- H.queryAll _item (H.request Item.GetValidity)
    H.modify_ _ { validity = foldMap identity vs }
    H.raise StatusUpdate

  Initialize -> do
    vs <- H.queryAll _item (H.request Item.GetValidity)
    H.modify_ _ { validity = foldMap identity vs }


handleQuery :: forall m a. MonadAff m => Query a -> ComponentM m (Maybe a)
handleQuery = case _ of
  GetCategories reply -> do
    result <- H.queryAll _item (H.request Item.GetCategory)
    pure $ Just $ reply $ Categories <<< Array.fromFoldable $ result

  GetValidity reply -> do
    v <- H.gets _.validity
    pure $ Just $ reply v
