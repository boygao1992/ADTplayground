module Polaris.UI.Component.DataTable.Product where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe, fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
-- import Halogen.HTML.Events as HE
-- import Halogen.HTML.Properties as HP
import Polaris.UI.Block.DataTable as DataTable

import Polaris.Data.Product (Product(..))

type State =
  { tableData :: Array Product
  }

defaultInitialState :: State
defaultInitialState =
  { tableData: []
  }

data Action
  = NoOp

type Query = Const Void

type Input = Array Product

type Output = Void

type ChildSlots = ()

type ComponentM m a = H.HalogenM State Action ChildSlots Output m a
type Component m = H.Component HH.HTML Query Input Output m
type ComponentHTML m = H.ComponentHTML Action ChildSlots m
type ComponentRender m = State -> ComponentHTML m

type Slot = H.Slot Query Output

component :: forall m. MonadAff m => Component m
component = H.mkComponent
  { initialState: \tableData ->
      defaultInitialState
        { tableData = tableData
        }
  , render
  , eval: H.mkEval $ H.defaultEval
      -- { handleAction = handleAction
      -- , handleQuery = handleQuery
      -- }
  }

render :: forall m. MonadAff m => ComponentRender m
render { tableData } = renderDataTable
  where
    renderDataTable :: forall p i. HH.HTML p i
    renderDataTable = DataTable.table_ $ [ renderHeader ] <> renderBody

    renderHeader :: forall p i. HH.HTML p i
    renderHeader =
      DataTable.row_
      [ DataTable.header_ [ HH.text "id"]
      , DataTable.header_ [ HH.text "title" ]
      , DataTable.header_ [ HH.text "vendor" ]
      , DataTable.header_ [ HH.text "product type" ]
      ]

    renderBody :: forall p i. Array (HH.HTML p i)
    renderBody = DataTable.row_ <$> ( renderData <$> tableData )

    renderData :: forall p i. Product -> Array (HH.HTML p i)
    renderData (Product { id, title, vendor, product_type }) =
      [ renderCell id
      , renderTextCell title
      , renderTextCell vendor
      , renderTextCell product_type
      ]

    renderCell :: forall a p i. Show a => Maybe a -> HH.HTML p i
    renderCell = DataTable.cell_ <<< (\x -> [x]) <<< HH.text <<< fromMaybe "" <<< map show

    renderTextCell :: forall p i. Maybe String -> HH.HTML p i
    renderTextCell = DataTable.cell_ <<< (\x -> [x]) <<< HH.text <<< fromMaybe ""

