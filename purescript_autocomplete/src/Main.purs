module Main where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff, delay, forkAff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Select (buildComponent, empty, defaultConfig, Query(..))
import Web.DOM.ParentNode (QuerySelector(..))


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

newtype Item = Item String
derive newtype instance eqItem :: Eq Item
derive newtype instance ordItem :: Ord Item
instance showItem :: Show Item where
  show (Item s) =  s

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  app <- HA.selectElement (QuerySelector "#app")
  io <- runUI
    ( buildComponent
        { toHTML : \(Item it) -> HH.text it
        , toId : \(Item it) -> it
        , toKeyword : \(Item it) -> it
        }
        { internal : empty
        , external : Just (map Item [ "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven" ])
        , config : defaultConfig
        }
    )
    []
    (fromMaybe body app)

  _ <- H.liftAff $ forkAff do
    delay (Milliseconds 10000.0)
    H.liftEffect $ log "10 second"
    io.query $ H.action $ Sync (map Item [ "zero", "ten", "eight", "nine", "five", "four", "two", "three", "eleven", "one"])

  pure unit
