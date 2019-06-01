module Magento.Import.UI.Container.Popup where

import Prelude

import Concurrent.BoundedQueue (BoundedQueue)
import Concurrent.BoundedQueue as BoundedQueue
import Concurrent.Queue (Queue)
import Concurrent.Queue as Queue
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Rec.Class (forever)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), isJust)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.Icon as Icon
import Ocelot.Block.Toast as Toast

data PopupStatus
  = Success
  | Error
derive instance genericPopupStatus :: Generic PopupStatus _
derive instance eqPopupStatus :: Eq PopupStatus
derive instance ordPopupStatus :: Ord PopupStatus
instance showPopupStatus :: Show PopupStatus where
  show = genericShow

type PopupPayload =
  { status :: PopupStatus
  , message :: String
  }

type State =
  { messageQueue :: Maybe (Queue PopupPayload)
  , popQueue :: Maybe (BoundedQueue PopupPayload)
  , displayQueue :: Maybe (BoundedQueue PopupPayload)
  , inDisplay :: Maybe PopupPayload
  }

defaultInitialState :: State
defaultInitialState =
  { messageQueue: Nothing
  , popQueue: Nothing
  , displayQueue: Nothing
  , inDisplay: Nothing
  }

data Action
  = Initialize

data Query a
  = PushNew PopupPayload a

type Input = Unit

type Output = Void

type ChildSlots = ()

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
      , handleQuery = handleQuery
      , initialize = Just Initialize
      }
  }

render :: forall m. MonadAff m => ComponentRender m
render { inDisplay } =
  Toast.toast
  [ Toast.visible $ isJust inDisplay ]
  case inDisplay of
    Nothing ->
      []
    Just { status, message } ->
      [ case status of
           Success ->
             Icon.success
             [ HP.classes $ HH.ClassName <$> ["text-green", "text-2xl", "mr-2"] ]
           Error ->
             Icon.error
             [ HP.classes $ HH.ClassName <$> ["text-red", "text-2xl", "mr-2"] ]
      , HH.p_
        [ HH.text message ]
      ]

handleAction :: forall m. MonadAff m => Action -> ComponentM m Unit
handleAction = case _ of

  Initialize -> do
    messageQueue <- H.liftAff $ Queue.new
    popQueue <- H.liftAff $ BoundedQueue.new 1
    displayQueue <- H.liftAff $ BoundedQueue.new 1
    H.modify_
      _ { messageQueue = Just messageQueue
        , popQueue = Just popQueue
        , displayQueue = Just displayQueue
        }

    void $ H.fork $ forever $ H.liftAff do
      item <- Queue.read messageQueue
      BoundedQueue.write popQueue item

    void $ H.fork $ forever $ H.liftAff do
      item <- BoundedQueue.read popQueue
      Aff.delay $ Aff.Milliseconds 300.0
      BoundedQueue.write displayQueue item

    void $ H.fork $ forever do
      item <- H.liftAff $ BoundedQueue.read displayQueue
      H.modify_ _ { inDisplay = Just item }
      H.liftAff $ Aff.delay $ Aff.Milliseconds 2000.0
      H.modify_ _ { inDisplay = Nothing }
      H.liftAff $ Aff.delay $ Aff.Milliseconds 200.0


handleQuery :: forall m a. MonadAff m => Query a -> ComponentM m (Maybe a)
handleQuery = case _ of
  PushNew item next -> Just next <$ runMaybeT do
    mq <- MaybeT $ H.gets _.messageQueue
    H.lift $ H.liftAff $ Queue.write mq item
