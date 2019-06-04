module Magento.Import.UI.Container.OptionPanel.Modal where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
-- import Halogen.Query.EventSource as HES
import Halogen.Util (debugShow)
import Magento.Import.Data.Options (Options)
import Magento.Import.UI.Container.OptionPanel.Options as Options
import Ocelot.Block.Button as Button
import Ocelot.Block.Format as Format
import Ocelot.Part.Modal as Modal
import Type.Data.Symbol (SProxy(..))
import Web.Event.Event as WE
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.KeyboardEvent as KE

type State =
  { sid :: Maybe H.SubscriptionId
  , initOptions :: Options
  }

defaultInitialState :: State
defaultInitialState =
  { sid: Nothing
  , initOptions: Options.defaultInitialState
  }

data Action
  = Initialize
  | CloseAndDiscard
  | CloseAndSave
  | HandleKey KE.KeyboardEvent
  | ClickStopPropagation ME.MouseEvent

type Query = Const Void

type Input = Options

data Output
  = Closed (Maybe Options)

type ChildSlots =
  ( options :: Options.Slot Unit
  )

_options = SProxy :: SProxy "options"

type ComponentM m a = H.HalogenM State Action ChildSlots Output m a
type Component m = H.Component HH.HTML Query Input Output m
type ComponentHTML m = H.ComponentHTML Action ChildSlots m
type ComponentRender m = State -> ComponentHTML m

type Slot = H.Slot Query Output

component :: forall m. MonadAff m => Component m
component = H.mkComponent
  { initialState: \options -> defaultInitialState { initOptions = options }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: forall m. MonadAff m => ComponentRender m
render st =
  Modal.modal CloseAndDiscard
  [ HE.onClick $ Just <<< ClickStopPropagation]
  [ Modal.header
    { buttons:
      [ HH.a [ HP.classes ( Format.linkDarkClasses <> [ HH.ClassName "mr-4" ] )
              , HE.onClick $ Just <<< const CloseAndDiscard
              ]
        [ HH.text "Cancel" ]
      , Button.buttonPrimary [ HE.onClick $ Just <<< const CloseAndSave ]
        [ HH.text "Save" ]
      ]
    , title:
      [ HH.text "Options" ]
    }
  , Modal.body_
    [ HH.slot _options unit Options.component st.initOptions (const Nothing)
    ]
  ]

handleAction :: forall m. MonadAff m => Action -> ComponentM m Unit
handleAction = case _ of
  Initialize -> do
    sid <- Modal.initializeWith (Just <<< HandleKey)
    H.modify_ _ { sid = Just sid }

  CloseAndDiscard -> do
    H.raise $ Closed Nothing

  HandleKey ke -> do
    msid <- H.gets _.sid
    for_ msid \sid -> do
      -- FIXME Escape not working
      Modal.whenClose ke sid (H.raise $ Closed Nothing)

  CloseAndSave -> do
    mr <- H.query _options unit $ H.request Options.GetAllOptions
    debugShow mr
    for_ mr \result ->
      H.raise $ Closed $ Just result

  ClickStopPropagation me -> do
    H.liftEffect $ WE.stopPropagation <<< ME.toEvent $ me


