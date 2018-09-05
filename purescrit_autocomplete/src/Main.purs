module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Foldable (class Foldable, foldl, traverse_)
import Effect (Effect)
import Effect.Console (log)
import Effect.Aff (Aff)
-- import Control.Coroutine as CR
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Query.EventSource as HQES
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.HTML (window) as DOM
import Web.HTML.Window (document) as DOM
import Web.HTML.HTMLDocument (HTMLDocument)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.Event.Event as WEE
import Web.Event.EventTarget as WEET
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

import Select (buildComponent, empty, defaultConfig)

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
  runUI
    ( buildComponent
        show
        { internal : empty
        , external : Just [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ]
        , config : defaultConfig
        }
    )
    []
    body

