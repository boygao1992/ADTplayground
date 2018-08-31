module Examples.KeyboardInput.Main where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Query.EventSource as HES
import Halogen.VDom.Driver (runUI)
import Web.Event.Event as E
import Web.Event.EventTarget as ET
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (HTMLDocument)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as DOM
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

type State = { chars :: String }

initialState :: State
initialState = { chars : "" }

-- Halogen/Query/HalogenM.purs
-- newtype SubscriptionId = SubscriptionId Int
-- derive newtype instance eqSubscriptionId :: Eq SubscriptionId
-- derive newtype instance ordSubscriptionId :: Ord SubscriptionId

data Query a
  = Init a
  | HandleKey KeyboardEvent (H.SubscribeStatus -> a)

type InMsg = Unit -- no input message from parent
type OutMsg = Void -- no output message to parent
type IO = Aff -- async IO

render :: State -> H.ComponentHTML Query
render state =
  HH.div_
    [ HH.p_ [ HH.text "Hold down the shift key and type some characters!" ]
    , HH.p_ [ HH.text "Press ENTER or RETURN to clear and remove the event listeners."]
    , HH.p_ [ HH.text state.chars ]
    ]

onKeyUp :: HTMLDocument -> (KeyboardEvent -> Effect Unit) -> Effect (Effect Unit)
onKeyUp document fn = do
  let target = HTMLDocument.toEventTarget document
  listener <- ET.eventListener (traverse_ fn <<< KE.fromEvent)
  ET.addEventListener KET.keyup listener false target
  pure $ ET.removeEventListener KET.keyup listener false target

eval :: Query ~> H.ComponentDSL State Query OutMsg IO
eval (Init next) = do
  -- DOM.window :: Effect Window
  -- DOM.document :: Window -> Effect HTMLDocument
  -- (=<<) is flipped bind (>>=)
  -- H.liftEffect :: forall a. MonadEffect HalogenM => Effect a -> HalogenM a
  document <- H.liftEffect $ DOM.document =<< DOM.window
  H.subscribe $ HES.eventSource' (onKeyUp document) (Just <<< H.request <<< HandleKey)
  pure next
eval (HandleKey ev reply)
  | KE.shiftKey ev = do
      H.liftEffect $ E.preventDefault (KE.toEvent ev)
      let char = KE.key ev
      when (String.length char == 1) do
        H.modify_ (\st -> st { chars = st.chars <> char })
      pure (reply H.Listening)
  | KE.key ev == "Enter" = do
      H.liftEffect $ E.preventDefault (KE.toEvent ev)
      H.modify_ (_ { chars = "" })
      pure (reply H.Done)
  | otherwise =
      pure (reply H.Listening)


ui :: H.Component HH.HTML Query InMsg OutMsg Aff
ui =
  H.lifecycleComponent
    { initialState : const initialState
    , render
    , eval
    , initializer : Just (H.action Init) -- send Init when initialized
    , finalizer : Nothing
    , receiver : const Nothing
    }

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI ui unit body
