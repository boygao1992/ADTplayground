module Magento.Import.UI.Page where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Read (read)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
-- import Halogen.HTML.Properties as HP
-- import Halogen.Query.EventSource as HES
import Halogen.Util (debugShow)
import Magento.Import.Data.Skus (Sku(..))
import Magento.Import.UI.Container.Categories as Categories
import Magento.Import.UI.Container.TabularFilePicker as FilePicker
import Magento.Import.UI.Container.Popup as Popup
import Ocelot.Block.Button (button) as Button
import Type.Data.Symbol (SProxy(..))

type State = Unit

initialState :: State
initialState = unit

data Action
  = GetAllValidCategories -- NOTE test
  | PopupTest Popup.PopupPayload -- NOTE test
  | HandleFilePicker FilePicker.Output

type Query = Const Void

type Input = Unit

type Output = Void

type ChildSlots =
  ( categories :: Categories.Slot Unit
  , filePicker :: FilePicker.Slot Unit
  , popup :: Popup.Slot Unit
  )

_categories = SProxy :: SProxy "categories"
_filePicker = SProxy :: SProxy "filePicker"
_popup = SProxy :: SProxy "popup"

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
  [ HH.slot _popup unit Popup.component unit (const Nothing)
  , HH.slot _filePicker unit FilePicker.component unit (Just <<< HandleFilePicker)
  , HH.slot _categories unit Categories.component
      [ Tuple (Sku "A")
        [ { category: fromMaybe mempty $ read "A/B/C"
          , validity: false
          }
          , { category: fromMaybe mempty $ read "A/E/F"
          , validity: true
          }
        ]
        , Tuple (Sku "M")
        [ { category: fromMaybe mempty $ read "D"
          , validity: true
          }
        , { category: fromMaybe mempty $ read "EF"
          , validity: false
          }
        ]
      ]
      (const Nothing)
  , Button.button
    [ HE.onClick $ Just <<< const GetAllValidCategories ]
    [ HH.text "getAllValidCategories" ]
  , Button.button
    [ HE.onClick $ Just <<< const (PopupTest { status: Popup.Success, message: "success"}) ]
    [ HH.text "Success" ]
  , Button.button
    [ HE.onClick $ Just <<< const (PopupTest { status: Popup.Error, message: "errorrrrrrrrrrrrrrrrrrrrr"}) ]
    [ HH.text "Error" ]
  ]

handleAction :: forall m. MonadAff m => Action -> ComponentM m Unit
handleAction = case _ of
  HandleFilePicker f -> case f of
    FilePicker.FileLoaded table -> do
      debugShow table
    FilePicker.Error err -> do
      void $ H.query _popup unit
        $ H.tell $ Popup.PushNew { status: Popup.Error , message: show err }

  GetAllValidCategories -> do
    result <- H.query _categories unit (H.request Categories.GetAllCategories)
    debugShow result

  PopupTest p -> do
    void $ H.query _popup unit (H.tell $ Popup.PushNew p)

