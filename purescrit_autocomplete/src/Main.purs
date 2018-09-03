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
import Halogen.VDom.Driver (runUI)

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

type InMsg = Unit

data OutMsg
  = WindowPushUpperBoundary
  | WindowPushLowerBoundary
  | Select Int

stateTransition :: Config -> Event -> State -> Tuple (State -> State) (Maybe OutMsg)
stateTransition config event state = case event of
  Keyboard subEvent ->
    keyboardTransition config subEvent state
  Scroll subEvent ->
    scrollTransition config subEvent state
  Mouse subEvent ->
    mouseTransition  config subEvent state
  Reset ->
    Tuple (const empty) Nothing

data KeyboardEvent
  = KeyUp
  | KeyDown

keyboardTransition
  :: forall r
   . Config ->
     KeyboardEvent ->
     { key :: Maybe Int, head :: Int | r } ->
     Tuple
       ({ key :: Maybe Int, head :: Int | r } ->
        { key :: Maybe Int, head :: Int | r }
       )
       (Maybe OutMsg)
keyboardTransition { windowSize, listSize } event { key, head } =
  case event of
  KeyUp ->
    case key of
    Nothing ->
      (_ { key = Just (head + windowSize - 1) }) ! Nothing
    Just keyPos ->
      let
        windowAtUpperBoundary h =
          h == 0
        aboveWindow k h =
          k - h < 0
      in
        case Tuple (windowAtUpperBoundary head) (aboveWindow (keyPos - 1) head) of
        Tuple true true  ->
          identity ! Just WindowPushUpperBoundary
        Tuple true false ->
          (_ { key = Just $ keyPos - 1 }) ! Nothing
        Tuple false true ->
          (_ { key = Just $ keyPos - 1, head = head - 1 }) ! Nothing
        Tuple false false ->
          (_ { key = Just $ keyPos - 1 }) ! Nothing

  KeyDown ->
    case key of
    Nothing ->
      (_ { key = Just head }) ! Nothing
    Just keyPos ->
      let
        windowAtLowerBoundary h =
          h == listSize - windowSize
        belowWindow k h =
          k - h >= windowSize
      in
        case Tuple (windowAtLowerBoundary head) (belowWindow (keyPos + 1) head) of
        Tuple true true ->
          identity ! Just WindowPushLowerBoundary
        Tuple true false ->
          (_ { key = Just $ keyPos + 1 }) ! Nothing
        Tuple false true ->
          (_ { key = Just $ keyPos + 1, head = head + 1 }) ! Nothing
        Tuple false false ->
          (_ { key = Just $ keyPos + 1 }) ! Nothing

data ScrollEvent
  = ScrollUp
  | ScrollDown

scrollTransition
  :: forall r
   . Config ->
     ScrollEvent ->
     { key :: Maybe Int, head :: Int | r } ->
     Tuple
       ({ key :: Maybe Int, head :: Int | r } ->
        { key :: Maybe Int, head :: Int | r }
       )
       (Maybe OutMsg)
scrollTransition { windowSize, listSize } event { key, head } =
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
        (_ { key = updateKey key newHead, head = newHead }) ! outMsg
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
        (_ { key = updateKey key newHead, head = newHead }) ! outMsg

data MouseEvent
  = MouseEnter Int
  | MouseLeave
  | MouseClick Int

mouseTransition
  :: forall r
   . Config ->
     MouseEvent ->
     { mouse :: Maybe Int | r } ->
     Tuple
       ({ mouse :: Maybe Int | r } ->
        { mouse :: Maybe Int | r }
       )
       (Maybe OutMsg)
mouseTransition _ event state@{ mouse } =
  case event of
    MouseEnter mousePos ->
      (_ { mouse = Just mousePos }) ! Nothing
    MouseLeave ->
      (_ { mouse = Nothing }) ! Nothing
    MouseClick mousePos ->
      (_ { mouse = Just mousePos }) ! Just (Select mousePos)

render :: State -> H.ComponentHTML Query
render state =
  HH.div_
    [ HH.div_ [ HH.text $ show state]
    , HH.div_
        [ HH.button
            [ HE.onClick $ HE.input_ $ Query $ Keyboard KeyUp ]
            [ HH.text "KeyUp"]
        , HH.button
            [ HE.onClick $ HE.input_ $ Query $ Keyboard KeyDown ]
            [ HH.text "KeyDown"]
        , HH.button
            [ HE.onClick $ HE.input_ $ Query $ Scroll ScrollUp ]
            [ HH.text "ScrollUp"]
        , HH.button
            [ HE.onClick $ HE.input_ $ Query $ Scroll ScrollDown ]
            [ HH.text "ScrollDown"]
        ]
    ]

eval :: forall m. Query ~> H.ComponentDSL State Query OutMsg m
eval (Query event next) = do
  state <- H.get
  let Tuple reducer outMsg = _stateTransition _config event state
  H.modify_ reducer
  case outMsg of
    Just msg -> H.raise msg
    Nothing -> pure unit
  pure next

  where
    _stateTransition = stateTransition
    _config = defaultConfig

ui :: forall m. H.Component HH.HTML Query InMsg OutMsg m
ui =
  H.component
    { initialState : const empty
    , render
    , eval
    , receiver : const Nothing
    }

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI ui unit body
