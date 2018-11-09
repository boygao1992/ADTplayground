module ViewComponent where

import Prelude

import CSS as CSS
import Colors as Colors
import Data.Int (toNumber) as Int
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (logShow) as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource (eventSource') as HES
-- import HalogenUtils (classList)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener) as WEE
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (HTMLDocument)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document, innerHeight) as DOM
import Web.UIEvent.WheelEvent (WheelEvent, deltaY, fromEvent) as WE
import Web.UIEvent.WheelEvent.EventTypes (wheel) as WE

-- | Effect
addWheelEventHandler :: HTMLDocument -> (WE.WheelEvent -> Effect Unit) -> Effect (Effect Unit)
addWheelEventHandler document fn = do
  let target = HTMLDocument.toEventTarget document
  listener <- WEE.eventListener (traverse_ fn <<< WE.fromEvent)
  WEE.addEventListener WE.wheel listener false target
  pure $ WEE.removeEventListener WE.wheel listener false target

-- | Types

type State =
  { initialY :: Number
  , deltaY :: Number
  , speed :: Number
  }

initialState :: State
initialState =
  { initialY : 0.0
  , deltaY : 0.0
  , speed : 1.1
  }

data Query next
  = Init next
  | OnScroll WE.WheelEvent (H.SubscribeStatus -> next)

type Input = Unit

type Output = Void

type IO = Aff

-- | View

-- | Component

scrollRef :: H.RefLabel
scrollRef = H.RefLabel "scroll-control"

render :: forall q. State -> H.ComponentHTML q
render state =
  HH.div_
  [ noise
  , noise
  , noise
  , noise
  , HH.div [ HC.style do
                CSS.position CSS.relative
                CSS.width $ CSS.px 100.0
                CSS.height $ CSS.px 100.0
                CSS.backgroundColor $ Colors.orange
                CSS.top $ CSS.px $ state.initialY + state.deltaY * state.speed
           , HP.ref scrollRef
           ]
    []
  ]

  where
    noise :: H.ComponentHTML q
    noise =
      HH.div_
      [ HH.p_
        [ HH.text "Setting the "
        , HH.code_
            [ HH.text "width" ]
        , HH.text " of a block-level element will prevent it from stretching out to the edges of its container to the left and right. Then, you can set the left and right margins to "
        , HH.code_
            [ HH.text "auto"]
        , HH.text " to horizontally center that element within its container. The element will take up the width you specify, then the remaining space will be split evenly between the two margins."
        ]
      , HH.p_
        [ HH.text "The only problem occurs when the browser window is narrower than the width of your element. The browser resolves this by creating a horizontal scrollbar on the page. Let's improve the situation..."
        ]
      ]

eval :: Query ~> H.ComponentDSL State Query Output IO
eval (Init next) = next <$ do
  document <- H.liftEffect $ DOM.document =<< DOM.window
  H.subscribe $ HES.eventSource' (addWheelEventHandler document) (Just <<< H.request <<< OnScroll)

  ms <- H.getHTMLElementRef scrollRef
  vh <- H.liftEffect $ DOM.innerHeight =<< DOM.window
  case ms of
    Just s -> do
      top <- H.liftEffect $ HTMLElement.offsetTop s
      speed <- H.gets _.speed
      let initialY = - speed * (top - (Int.toNumber vh))
      H.modify_ _ { initialY = 0.0} -- TODO: DeltaMode = Line, need conversion
      H.liftEffect $ Console.logShow $ initialY
    Nothing ->
      pure unit
eval (OnScroll ev reply) = do
  deltaY <- H.gets _.deltaY
  H.modify_ _ { deltaY =  max 0.0 $ deltaY + WE.deltaY ev}
  H.liftEffect $ Console.logShow $ max 0.0 $ deltaY + WE.deltaY ev
  pure $ reply H.Listening



component :: HTMLElement.HTMLElement -> H.Component HH.HTML Query Input Output IO
component body = H.lifecycleComponent spec'
  where
    spec' :: H.LifecycleComponentSpec HH.HTML State Query Input Output IO
    spec' =
      { initialState : const initialState
      , render
      , eval
      , initializer : Just (H.action Init)
      , finalizer : Nothing
      , receiver : const Nothing
      }
