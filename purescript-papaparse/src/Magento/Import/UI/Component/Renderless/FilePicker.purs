module Magento.Import.UI.Component.Renderless.FilePicker where

import Prelude

import Renderless.State (Store, extract, getsState, modifyState_, store)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.Traversable (for_)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Query.EventSource as HES
import Simple.JSON as JSON
import Web.Event.Event as WE
import Web.File.Blob (Blob, type_) as File
import Web.File.File (toBlob) as File
import Web.File.FileList (item) as File
import Web.File.FileReader as FR
import Web.HTML.Event.EventTypes as ET
import Web.HTML.HTMLInputElement as HIE

type State =
  { fileReader :: Maybe FR.FileReader
  , inputElement :: Maybe HIE.HTMLInputElement
  , mediaType :: Maybe MediaType
  }

type StateStore m
  = Store State (ComponentHTML m)

initialState :: State
initialState =
  { fileReader: Nothing
  , inputElement: Nothing
  , mediaType: Nothing
  }

data Action
  = Initialize
  | SetFile HIE.HTMLInputElement
  | LoadFile FR.FileReader

data Query a
  = Reset a

type Input m =
  { render :: ComponentRender m }

data Output
  = FileLoaded { data :: String, mediaType :: MediaType }

type ChildSlots = ()

type ComponentM m a = H.HalogenM (StateStore m) Action ChildSlots Output m a
type Component m = H.Component HH.HTML Query (Input m) Output m
type ComponentHTML m = H.ComponentHTML Action ChildSlots m
type ComponentRender m = State -> ComponentHTML m

type Slot = H.Slot Query Output

component :: forall m. MonadAff m => Component m
component = H.mkComponent
  { initialState: \({ render }) -> store render initialState
  , render: extract
  , eval: H.mkEval $ H.defaultEval
      { initialize = Just Initialize
      , handleAction = handleAction
      , handleQuery = handleQuery
      }
  }

handleQuery :: forall m a. MonadAff m => Query a -> ComponentM m (Maybe a)
handleQuery = case _ of
  Reset next -> Just next <$ do
    mie <- getsState _.inputElement
    for_ mie \ie ->
      H.liftEffect $ HIE.setValue "" ie
    modifyState_ _ { mediaType = Nothing }

handleAction :: forall m. MonadAff m => Action -> ComponentM m Unit
handleAction = case _ of
  Initialize -> do
    fr <- H.liftEffect FR.fileReader
    modifyState_ _ { fileReader = Just fr }
    void
      $ H.subscribe
      $ HES.eventListenerEventSource ET.load (FR.toEventTarget fr)
        (map LoadFile <<< FR.fromEventTarget <=< WE.target)

  SetFile ie -> do
    modifyState_ _ { inputElement = Just ie }
    mfs <- H.liftEffect $ HIE.files ie
    let
      (mb :: Maybe File.Blob)
        = do
          fs <- mfs
          f <- File.item 0 fs
          pure $ File.toBlob f
    for_ mb \(b :: File.Blob) -> do
      modifyState_ _ { mediaType = File.type_ b }
      mfr <- getsState _.fileReader
      for_ mfr \(fr :: FR.FileReader) -> do
        H.liftEffect $ FR.readAsText b fr

  LoadFile fr -> do
    fd <- H.liftEffect $ FR.result fr
    let (ms :: Maybe String) = JSON.read_ fd
    mmt <- getsState _.mediaType
    for_ ({data: _, mediaType: _} <$> ms <*> mmt) \result ->
      H.raise $ FileLoaded result
