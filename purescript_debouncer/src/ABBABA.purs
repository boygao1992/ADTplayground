module ABBABA where

import Prelude

import Data.Ord (abs)
import Data.Array as Array
import Data.DateTime.Instant (unInstant)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (toChar, toCharArray)
import Data.String.Common (toUpper)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Effect.Aff (Aff)
import Effect.Now (now)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event as WEE
import Web.UIEvent.KeyboardEvent as KE

type TimeStamp = Milliseconds
type State =
  { buffer :: Array (Tuple Char TimeStamp) -- internal state, sliding buffer
  , size :: Int -- config
  , secret :: Array Char -- config
  , maxDuration :: Milliseconds
  , success :: Boolean
  }

initialState :: State
initialState =
  { buffer : []
  , size : 6
  , secret : toCharArray "ABBABA"
  , maxDuration : Milliseconds 3000.0
  , success : false
  }

data Query next
  = OnKeyDown KE.KeyboardEvent next

type Input = Unit
type Output = Void

type IO = Aff

render :: State -> H.ComponentHTML Query
render { buffer, success } =
  HH.div_
    [ HH.div_
        [ HH.text $ show { buffer, success } ]
    , HH.input
        [ HE.onKeyDown $ HE.input OnKeyDown]
    ]

eval :: Query ~> H.ComponentDSL State Query Output IO
eval (OnKeyDown ke next) = next <$ do
  H.liftEffect <<< WEE.preventDefault <<< KE.toEvent $ ke
  case toChar(toUpper(KE.key ke)) of
    Just x | x == 'A' || x == 'B' -> do
      ms <- unInstant <$> H.liftEffect now
      { buffer, size, secret, maxDuration : (Milliseconds t) } <- H.get
      let newBuffer = fromMaybe buffer $ slide size buffer (Tuple x ms)
          withinDuration =
            fromMaybe false $
            (\(Milliseconds t1) (Milliseconds t2) -> abs(t1 - t2) < t)
            <$> (Tuple.snd <$> Array.head newBuffer)
            <*> (Tuple.snd <$> Array.last newBuffer)
      if (Tuple.fst <$> newBuffer) == secret && withinDuration
        then H.modify_ _ { buffer = [], success = true }
        else H.modify_ _ { buffer = newBuffer }
    _ -> do
      pure unit

component :: H.Component HH.HTML Query Input Output IO
component =
  H.component
    { initialState : const initialState
    , render
    , eval
    , receiver : const Nothing
    }

-- | Utils
slide :: forall a. Int -> Array a -> a -> Maybe (Array a)
slide size xs x
  | size < 1 = Nothing
  | Array.length xs + 1 <= size
    = Just $ Array.snoc xs x
  | Array.length xs == size
    = map (flap(Array.snoc) x) <<< Array.tail $ xs
  | otherwise = Nothing
