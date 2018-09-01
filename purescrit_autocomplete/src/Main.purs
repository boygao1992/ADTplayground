module Main where

import Prelude

import Data.Foldable (class Foldable, fold)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

withCmds :: forall model cmd. model -> cmd -> Tuple model cmd
withCmds model cmd =
  Tuple model cmd

infixl 5 withCmds as !


type Config =
  { windowSize :: Int
  , listSize :: Int
  }

defaultConfig :: Config
defaultConfig =
  { windowSize : 3
  , listSize : 10
  }

type State =
  { key :: Maybe Int
  , mouse :: Maybe Int
  , head :: Int
  }

empty :: State
empty =
  { key : Nothing
  , mouse : Nothing
  , head : 0
  }

data Query a
  = Query Event a

data Event
  = Keyboard KeyboardEvent
  | Scroll ScrollEvent
  | Mouse MouseEvent
  | Reset

data OutMsg
  = WindowPushUpperBoundary
  | WindowPushLowerBoundary
  | Select Int

stateTransition :: Config -> Event -> State -> Tuple State (Maybe OutMsg)
stateTransition config event state = case event of
  Keyboard subEvent ->
    keyboardTransition config subEvent state
  Scroll subEvent ->
    scrollTransition config subEvent state
  Mouse subEvent ->
    mouseTransition  config subEvent state
  Reset ->
    Tuple empty Nothing

data KeyboardEvent
  = KeyUp
  | KeyDown

keyboardTransition
  :: forall r
   . Config ->
     KeyboardEvent ->
     { key :: Maybe Int, head :: Int | r } ->
     Tuple { key :: Maybe Int, head :: Int | r } (Maybe OutMsg)
keyboardTransition { windowSize, listSize } event state@{ key, head } =
  case event of
  KeyUp ->
    case key of
    Nothing ->
      state { key = Just (head + windowSize - 1) } ! Nothing
    Just keyPos ->
      let
        windowAtUpperBoundary h =
          h == 0
        aboveWindow k h =
          k - h < 0
      in
        case Tuple (windowAtUpperBoundary head) (aboveWindow (keyPos - 1) head) of
        Tuple true true  ->
          state ! Just WindowPushUpperBoundary
        Tuple true false ->
          state { key = Just $ keyPos - 1 } ! Nothing
        Tuple false true ->
          state { key = Just $ keyPos - 1, head = head - 1 } ! Nothing
        Tuple false false ->
          state { key = Just $ keyPos - 1 } ! Nothing

  KeyDown ->
    case key of
    Nothing ->
      state { key = Just head } ! Nothing
    Just keyPos ->
      let
        windowAtLowerBoundary h =
          h == listSize - windowSize
        belowWindow k h =
          k - h >= windowSize
      in
        case Tuple (windowAtLowerBoundary head) (belowWindow (keyPos + 1) head) of
        Tuple true true ->
          state ! Just WindowPushLowerBoundary
        Tuple true false ->
          state { key = Just $ keyPos + 1 } ! Nothing
        Tuple false true ->
          state { key = Just $ keyPos + 1, head = head + 1 } ! Nothing
        Tuple false false ->
          state { key = Just $ keyPos + 1 } ! Nothing

data ScrollEvent
  = ScrollUp
  | ScrollDown

scrollTransition
  :: forall r
   . Config ->
     ScrollEvent ->
     { key :: Maybe Int, head :: Int | r } ->
     Tuple { key :: Maybe Int, head :: Int | r } (Maybe OutMsg)
scrollTransition { windowSize, listSize } event state@{ key, head } =
  let
    keyOutOfWindow k h =
      k < h || k >= h + windowSize
    updateKey :: Maybe Int -> Int -> Maybe Int
    updateKey Nothing  _ = Nothing
    updateKey (Just keyPos) h
      | keyOutOfWindow keyPos h = Nothing
      | otherwise = Just keyPos
  in
    case event of
    ScrollUp ->
      let
        windowAtUpperBoundary h =
          h == 0
        Tuple newHead outMsg =
          if windowAtUpperBoundary head then
            Tuple head (Just WindowPushUpperBoundary)
          else
            Tuple (head - 1) Nothing
      in
        state { key = updateKey key newHead, head = newHead } ! outMsg
    ScrollDown ->
      let
        windowAtLowerBoundary h =
          h == listSize - windowSize
        Tuple newHead outMsg =
          if windowAtLowerBoundary head then
            Tuple head (Just WindowPushLowerBoundary)
          else
            Tuple (head + 1) Nothing
      in
        state { key = updateKey key newHead, head = newHead } ! outMsg

data MouseEvent
  = MouseEnter Int
  | MouseLeave
  | MouseClick Int

mouseTransition
  :: forall r
   . Config ->
     MouseEvent ->
     { mouse :: Maybe Int | r } ->
     Tuple { mouse :: Maybe Int | r } (Maybe OutMsg)
mouseTransition _ event state@{ mouse } =
  case event of
    MouseEnter mousePos ->
      state { mouse = Just mousePos } ! Nothing
    MouseLeave ->
      state { mouse = Nothing } ! Nothing
    MouseClick mousePos ->
      state { mouse = Just mousePos } ! Just (Select mousePos)


-- main :: Effect Unit
-- main = do
--   log "Hello sailor!"
