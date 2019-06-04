module Magento.Import.UI.Container.TabularFilePicker where

import Prelude

import Control.Monad.Maybe.Trans (runMaybeT)
import Control.MonadZero (empty)
import Data.Array as Array
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType.Extra as MediaType
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
-- import Halogen.HTML.Events as HE
-- import Halogen.HTML.Properties as HP
import Halogen.Util (debugShow)
import Magento.Import.UI.Component.FilePicker as FilePicker
import Magento.Import.UI.Data.File (File(..))
import PapaParse as CSV
import XLSX as XLSX

type State = Unit

defaultInitialState :: State
defaultInitialState = unit

data Action
  = HandleFilePicker FilePicker.Output

type Query = Const Void

type Input = Unit

data Output
  = FileLoaded File
  | Error ErrorType

data ErrorType
  = UnsupportedFileType
  | XLSXParsingError String
  | CSVParsingError (Array CSV.ParseError)

instance showErrorType :: Show ErrorType where
  show UnsupportedFileType = "Unsupported file type"
  show (XLSXParsingError err) = "XLSX parsing error: " <> err
  show (CSVParsingError arr) = "CSV parsing error: " <> (fromMaybe "unknown" $ show <$> Array.head arr)

type ChildSlots =
  ( filePicker :: FilePicker.Slot Unit
  )

type ComponentM m a = H.HalogenM State Action ChildSlots Output m a
type Component m = H.Component HH.HTML Query Input Output m
type ComponentHTML m = H.ComponentHTML Action ChildSlots m
type ComponentRender m = State -> ComponentHTML m

type Slot = H.Slot Query Output

component :: forall m. MonadAff m => Component m
component = H.mkComponent
  { initialState: const defaultInitialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      }
  }


render :: forall m. MonadAff m => ComponentRender m
render _ =
  HH.div_
  [ HH.slot FilePicker._filePicker unit FilePicker.component
      { render: FilePicker.simpleRender} (Just <<< HandleFilePicker)
  ]

handleAction :: forall m. MonadAff m => Action -> ComponentM m Unit
handleAction = case _ of
  HandleFilePicker event -> case event of
    FilePicker.FileLoaded f -> void $ runMaybeT do
      H.lift $ debugShow $ f.mediaType
      when (f.mediaType /= MediaType.applicationXLSX
            && f.mediaType /= MediaType.textCSV) do
        H.lift $ H.raise $ Error UnsupportedFileType
        empty

      csv <- if (f.mediaType == MediaType.applicationXLSX)
        then do
          eCSV <- H.lift $ H.liftEffect $ XLSX.toCSV f.data
          case eCSV of
            Left err -> do
              H.lift $ H.raise $ Error $ XLSXParsingError err
              empty
            Right csv -> pure csv
        else
          pure f.data

      eFile <- H.lift $ H.liftEffect $ CSV.parseFile csv
      H.lift $ H.raise $ case eFile of
        Left err -> Error $ CSVParsingError err
        Right file -> FileLoaded $ File file

