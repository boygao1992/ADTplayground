module Examples.DriverWebsocket.Main where

import Prelude

import Control.Applicative (class Applicative)
import Control.Coroutine as CR
import Control.Coroutine.Aff (emit)
import Control.Coroutine.Aff as CRA
import Control.Monad (class Monad)
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Foldable (class Foldable, for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (F, Foreign, readString, unsafeToForeign)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Web.Event.EventTarget as EET
import Web.Event.Internal.Types (Event)
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.WebSocket as WS

import Log as Log

wsProducer :: WS.WebSocket -> CR.Producer String Aff Unit
wsProducer socket = CRA.produce emitter_

  where
    emitter_ :: forall r. CRA.Emitter Effect String r -> Effect Unit -- a = String, r = Unit
    emitter_ emitter = do
      listener <- EET.eventListener eventHandler
      EET.addEventListener
        WSET.onMessage
        listener
        false
        (WS.toEventTarget socket)

      where
        eventHandler :: Event -> Effect Unit
        eventHandler ev = do
          -- for_ :: Maybe ME.MessageEvent -> (ME.MessageEvent -> Effect Unit) -> Effect Unit
          -- ME.fromEvent :: Event -> Maybe ME.MessageEvent
          for_ (ME.fromEvent ev) msgEventHandler

        msgEventHandler :: ME.MessageEvent -> Effect Unit -- m = Effect
        msgEventHandler msgEvent =
          -- for_ :: Maybe String -> (String -> Effect Unit) -> Effect Unit
          for_ (readerHelper readString (ME.data_ msgEvent)) msgHandler

        readerHelper :: (Foreign -> F String) -> Foreign -> Maybe String
        readerHelper read =
          either (const Nothing) Just
                -- runExcept :: forall e a. Except e a -> Either e a -- a = String
            <<< runExcept
                -- read :: Foreign -> F a -- a = String
            <<< read
                -- unsafeToForeign :: forall b. b -> Foreign
            <<< unsafeToForeign

        msgHandler :: String -> Effect Unit
        msgHandler msg =
          CRA.emit emitter msg

wsConsumer :: (Log.Query ~> Aff) -> CR.Consumer String Aff Unit
wsConsumer query = CR.consumer msgHandler
  where
    msgHandler :: String -> Aff (Maybe Unit)
    msgHandler msg = do
      query $ H.action $ Log.AddMessage msg
      pure Nothing
wsSender :: WS.WebSocket -> CR.Consumer Log.Output Aff Unit
wsSender socket = CR.consumer msgHandler
  where
    msgHandler :: Log.Output -> Aff (Maybe Unit)
    msgHandler (Log.OutMessage msgContents) = do
      H.liftEffect $ WS.sendString socket msgContents
      pure Nothing
