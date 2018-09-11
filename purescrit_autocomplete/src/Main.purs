module Main where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Data.Foldable (class Foldable, foldl, traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Fiber, delay, error, forkAff, killFiber)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as HQES
import Halogen.VDom.Driver (runUI)
import Partial.Unsafe (unsafePartial)
import Select (buildComponent, empty, defaultConfig, Query(..))
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Event.Event as WEE
import Web.Event.EventTarget as WEET
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (HTMLDocument)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as DOM
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

-- data Query next
  -- | Init next
  -- | HandleKey KE.KeyboardEvent (H.SubscribeStatus -> next)

type InMsg = Unit

data OutMsg = Void

type IO = Aff

-- onKeyUp_ :: HTMLDocument -> (KE.KeyboardEvent -> Effect Unit) -> Effect (Effect Unit)
-- onKeyUp_ document fn = do
--   let target = HTMLDocument.toEventTarget document
--   listener <- WEET.eventListener (traverse_ fn <<< KE.fromEvent)
--   WEET.addEventListener KET.keyup listener false target
--   pure $ WEET.removeEventListener KET.keyup listener false target

-- onKeyDown_ :: HTMLDocument -> (KE.KeyboardEvent -> Effect Unit) -> Effect (Effect Unit)
-- onKeyDown_ document fn = do
--   let target = HTMLDocument.toEventTarget document
--   listener <- WEET.eventListener (traverse_ fn <<< KE.fromEvent)
--   WEET.addEventListener KET.keydown listener false target
--   pure $ WEET.removeEventListener KET.keydown listener false target

-- eval :: forall f item. Query ~> H.ComponentDSL (State f item) Query OutMsg IO
-- eval (Init next) = do
--   document <- H.liftEffect $ DOM.document =<< DOM.window
--   H.subscribe $ HQES.eventSource' (onKeyUp_ document) (Just <<< H.request <<< HandleKey)
--   H.subscribe $ HQES.eventSource' (onKeyDown_ document) (Just <<< H.request <<< HandleKey)
--   pure next
-- eval (HandleKey ev reply)
--   = do
--     H.liftEffect $ WEE.preventDefault $ KE.toEvent ev
--     H.liftEffect $ log $ KE.key ev
--     pure (reply H.Listening)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  app <- HA.selectElement (QuerySelector "#app")
  io <- runUI
    ( buildComponent
        show
        { internal : empty
        , external : Just [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ]
        , config : defaultConfig
        }
    )
    []
    (fromMaybe body app)

  _ <- H.liftAff $ forkAff do
    delay (Milliseconds 2000.0)
    H.liftEffect $ log "2 second"
    io.query $ H.action $ Sync [11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]

  pure unit



