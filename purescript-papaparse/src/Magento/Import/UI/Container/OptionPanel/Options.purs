module Magento.Import.UI.Container.OptionPanel.Options where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Magento.Import.Data.Options (Options, OptionId, defaultOptions)
import Magento.Import.UI.Component.Checkbox (ComponentRender, Query(..), Output(..), Slot, _checkbox, component, setProps) as Checkbox
import Ocelot.Block.Card as Card
import Ocelot.Block.Checkbox (checkbox) as Checkbox
import Ocelot.Block.Format as Format
import Ocelot.HTML.Properties (css)

type State = Options

defaultInitialState :: State
defaultInitialState = defaultOptions

data Action
  = HandleCheckbox OptionId Checkbox.Output

data Query a
  = GetAllOptions (Options -> a)

type Input = Options

data Output
  = OptionChanged OptionId Boolean

type ChildSlots =
  ( checkbox :: Checkbox.Slot OptionId
  )

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
      { handleAction = handleAction
      , handleQuery = handleQuery
      }
  }

render :: forall m. MonadAff m => ComponentRender m
render st =
  Card.card [ HP.class_ $ HH.ClassName "flex-1 m-10" ]
  [ HH.h3 [ HP.classes Format.captionClasses ]
    [ HH.text "Attributes to be validated" ]
  , HH.div [ css "flex" ]
    [ renderOption "sku" "Sku"
    , renderOption "category" "Category"
    , renderOption "product_type" "Product Type"
    ]
  ]

  where
    renderCheckbox :: String -> Checkbox.ComponentRender m
    renderCheckbox label b =
      Checkbox.checkbox [ css "pr-6" ]
                        (Checkbox.setProps b [])
      [ HH.text label ]

    renderOption :: OptionId -> String -> ComponentHTML m
    renderOption id label =
      HH.slot Checkbox._checkbox id Checkbox.component
        { render: renderCheckbox label
        , initialState: fromMaybe true $ Map.lookup id st
        }
        (Just <<< HandleCheckbox id)

handleAction :: forall m. MonadAff m => Action -> ComponentM m Unit
handleAction = case _ of
  HandleCheckbox label (Checkbox.Checked b)-> do
    H.raise $ OptionChanged label b

handleQuery :: forall m a. MonadAff m => Query a -> ComponentM m (Maybe a)
handleQuery = case _ of
  GetAllOptions reply -> do
    result <- H.queryAll Checkbox._checkbox (H.request Checkbox.Get)
    pure $ Just $ reply result
