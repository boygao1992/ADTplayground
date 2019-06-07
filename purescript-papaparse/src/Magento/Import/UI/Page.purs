module Magento.Import.UI.Page where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Const (Const)
import Data.Either (hush) as Either
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Util (debug, debugShow)
import Magento.Import.Data.Categories (VerifiedCategory, columnName) as Categories
import Magento.Import.Data.Options (Options, defaultOptions)
import Magento.Import.Data.Skus (Sku)
import Magento.Import.UI.Api.Category as CategoryApi
import Magento.Import.UI.Container.Categories (Query(..), Slot, component) as Categories
import Magento.Import.UI.Container.OptionPanel as OptionPanel
import Magento.Import.UI.Container.Popup as Popup
import Magento.Import.UI.Container.TabularFilePicker as FilePicker
import Magento.Import.UI.Data.File (File, RowTableHeaded, _parseRowTableHeaded)
import Magento.Import.UI.Data.File as File
import Ocelot.Block.Button (button) as Button
import Type.Data.Symbol (SProxy(..))

type State =
  { options :: Options
  , file :: Maybe File
  , rowTable :: Maybe RowTableHeaded
  , categoryValidationResult
    :: Maybe (Array (Tuple Sku (Array Categories.VerifiedCategory)))
  }

initialState :: State
initialState =
  { options: defaultOptions
  , file: Nothing
  , rowTable: Nothing
  , categoryValidationResult: Nothing
  }

data Action
  = GetAllValidCategories -- NOTE test
  | PopupTest Popup.PopupPayload -- NOTE test
  | HandleFilePicker FilePicker.Output
  | OpenOptionPanel
  | HandleOptionPanel OptionPanel.Output

type Query = Const Void

type Input = Unit

type Output = Void

type ChildSlots =
  ( categories :: Categories.Slot Unit
  , filePicker :: FilePicker.Slot Unit
  , popup :: Popup.Slot Unit
  , optionPanel :: OptionPanel.Slot Unit
  )

_categories = SProxy :: SProxy "categories"
_filePicker = SProxy :: SProxy "filePicker"
_popup = SProxy :: SProxy "popup"
_optionPanel = SProxy :: SProxy "optionPanel"

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
render st =
  HH.div_
  [ HH.slot _popup unit Popup.component unit (const Nothing)
  , HH.slot _optionPanel unit OptionPanel.component unit (Just <<< HandleOptionPanel)
  , Button.button
    [ HE.onClick $ Just <<< const OpenOptionPanel ]
    [ HH.text "Open Option Panel" ]
  , HH.slot _filePicker unit FilePicker.component unit (Just <<< HandleFilePicker)
  , case st.categoryValidationResult of
      Nothing -> HH.text ""
      Just categoryValidationResult ->
        HH.slot _categories unit Categories.component
          { initCategories: categoryValidationResult
          , visibility: fromMaybe true $ Map.lookup "category" st.options
          }
          (const Nothing)
  , Button.button
    [ HE.onClick $ Just <<< const GetAllValidCategories ]
    [ HH.text "getAllValidCategories" ]
  , Button.button
    [ HE.onClick $ Just <<< const (PopupTest { status: Popup.Success, message: "success"}) ]
    [ HH.text "Success" ]
  , Button.button
    [ HE.onClick $ Just <<< const (PopupTest { status: Popup.Warning, message: "warning"}) ]
    [ HH.text "Warning" ]
  , Button.button
    [ HE.onClick $ Just <<< const (PopupTest { status: Popup.Error, message: "errorrrrrrrrrrrrrrrrrrrrr"}) ]
    [ HH.text "Error" ]
  ]

handleAction :: forall m. MonadAff m => Action -> ComponentM m Unit
handleAction = case _ of
  HandleFilePicker f -> case f of
    FilePicker.FileLoaded file -> do
      H.modify_ _ { file = Just file }
      case File.parseRowTable file of
        Nothing -> do
          void $ H.query _popup unit $ H.tell
            $ Popup.PushNew { status: Popup.Error , message: show "Failure when parsing File" }
        Just rowTable -> do
          debug "HandleFilePicker > FileLoaded > parseRowTable"
          -- NOTE debug
          mCategoryValidationResult <- runMaybeT $ do
            rowTableHeaded <- MaybeT $ pure $ _parseRowTableHeaded rowTable
            categoryColumn <- MaybeT $ pure $ File.parseSingleColumn Categories.columnName rowTableHeaded
            result <- MaybeT $ H.liftAff
              $ map (Either.hush <<< _.body) <<< CategoryApi.batchValidate
              $ categoryColumn
            pure $ CategoryApi.parseBatchValidationResult result rowTableHeaded
          H.modify_ _ { categoryValidationResult = mCategoryValidationResult }

    FilePicker.Error err -> do
      void $ H.query _popup unit
        $ H.tell $ Popup.PushNew { status: Popup.Error , message: show err }

  GetAllValidCategories -> do
    result <- H.query _categories unit (H.request Categories.GetAllCategories)
    debugShow result

  PopupTest p -> do
    void $ H.query _popup unit (H.tell $ Popup.PushNew p)

  OpenOptionPanel -> do
    void $ H.query _optionPanel unit (H.tell $ OptionPanel.Open)

  HandleOptionPanel (OptionPanel.OptionChanged opts) -> do
    H.modify_ _ { options = opts }

