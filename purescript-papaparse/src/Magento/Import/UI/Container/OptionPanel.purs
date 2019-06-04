module Magento.Import.UI.Container.OptionPanel where

import Prelude


import Data.Maybe (Maybe(..))

import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
-- import Halogen.HTML.Events as HE
-- import Halogen.HTML.Properties as HP
import Magento.Import.Data.Options (Options, defaultOptions)
import Magento.Import.UI.Container.OptionPanel.Modal as Modal
import Type.Data.Symbol (SProxy(..))

type State =
  { options :: Options
  , modalStatus :: Boolean
  }

defaultInitialState :: State
defaultInitialState =
  { options: defaultOptions
  , modalStatus: false
  }

data Action
  = HandleModal Modal.Output

data Query a
  = GetOptions (Options -> a)
  | Open a

type Input = Unit

data Output
  = OptionChanged Options

type ChildSlots =
  ( modal :: Modal.Slot Unit
  )

_modal = SProxy :: SProxy "modal"

type ComponentM m a = H.HalogenM State Action ChildSlots Output m a
type Component m = H.Component HH.HTML Query Input Output m
type ComponentHTML m = H.ComponentHTML Action ChildSlots m
type ComponentRender m = State -> ComponentHTML m

type Slot = H.Slot Query Output

component :: forall m. MonadAff m => Component m
component = H.mkComponent
  { initialState: const defaultInitialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      }
  }

render :: forall m. MonadAff m => ComponentRender m
render st =
  if st.modalStatus
  then HH.slot _modal unit Modal.component st.options (Just <<< HandleModal)
  else HH.text ""

handleAction :: forall m. MonadAff m => Action -> ComponentM m Unit
handleAction = case _ of
  HandleModal (Modal.Closed mo) -> do
    H.modify_ _ { modalStatus = false }
    case mo of
      Just opts -> do
        H.modify_
          _ { options = opts }
        H.raise $ OptionChanged opts
      Nothing -> pure unit

handleQuery :: forall m a. MonadAff m => Query a -> ComponentM m (Maybe a)
handleQuery = case _ of
  GetOptions reply -> do
    opts <- H.gets _.options
    pure $ Just $ reply opts

  Open next -> Just next <$ do
    H.modify_ _ { modalStatus = true }
