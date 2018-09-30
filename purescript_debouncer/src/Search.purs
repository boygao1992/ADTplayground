module Search where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Fiber, Aff)
import Effect.Aff as Aff
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- | Configuration
-- type Config =
--   { debounceTime :: Milliseconds
--   }

-- | Halogen Component Type
type State =
  { search :: String -- internal state, can be moved to Output
  , debouncer :: Maybe Debouncer -- internal state
  , debounceTime :: Milliseconds -- config
  , inputText :: String -- external state
  }

initialState :: State
initialState =
  { search : ""
  , debouncer : Nothing
  , debounceTime : Milliseconds 1000.0
  , inputText : ""
  }

data Query next
  = Search String next

type Input = Unit

type Output = Void

type IO = Aff

-- | Debouncer
type Debouncer =
  { channel :: AVar Unit
  , fiber :: Fiber Unit
  }

eval :: Query ~> H.ComponentDSL State Query Output IO
eval (Search s next) = next <$ do
  { debouncer, debounceTime } <- H.get
  H.modify_ _ { inputText = s }
  case debouncer of
    Nothing -> do
      channel <- H.liftAff AVar.empty
      fiber <- H.liftAff $ Aff.forkAff do
        Aff.delay debounceTime
        AVar.put unit channel
      _ <- H.fork do
        _ <- H.liftAff $ AVar.take channel
        text <- H.gets _.inputText
        H.modify_ _ { debouncer = Nothing, search = text }
      H.modify_ _ { debouncer = Just { channel, fiber } }
    Just { channel, fiber } -> do
      _ <- H.liftAff $ Aff.killFiber (Aff.error "renew timer") fiber
      newFiber <- H.liftAff $ Aff.forkAff do
        Aff.delay debounceTime
        AVar.put unit channel
      H.modify_ _ { debouncer = Just { channel, fiber : newFiber } }

render :: State -> H.ComponentHTML Query
render { search } =
  HH.div_
    [ HH.div_
        [ HH.text $ show { search } ]
    , HH.input
        [ HE.onValueInput $ HE.input Search
        , HP.value $ search
        ]
    ]

component
  :: H.Component HH.HTML Query Input Output IO
component =
  H.component
    { initialState : const initialState
    , render
    , eval
    , receiver : const Nothing
    }
