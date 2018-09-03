module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Foldable (class Foldable, foldl)
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
-- import Halogen.HTML.Properties as HP
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

type InternalState =
  { key :: Maybe Int
  , mouse :: Maybe Int
  , head :: Int
  }

type State f a =
  { internal :: InternalState
  , external :: Maybe (f a)
  }

empty :: InternalState
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

stateTransition :: Config -> Event -> InternalState -> Tuple (InternalState -> InternalState) (Maybe OutMsg)
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

buildRender :: forall f a. Foldable f => (a -> String) -> State f a -> H.ComponentHTML Query
buildRender li { internal , external } =
  HH.div_
    [ HH.div_ [ HH.text $ show internal ]
    , toUl external
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
  where
    toUl :: Maybe (f a) -> H.ComponentHTML Query
    toUl e
      | Just x <- e = HH.div_
        [ HH.ul_ $
            foldl (\acc a -> acc <> [ HH.li_ [ HH.text $ li a ] ]) [] x ]
      | otherwise = HH.div_ []

eval :: forall f a m. Query ~> H.ComponentDSL (State f a) Query OutMsg m
eval (Query event next) = do
  internalState <- H.gets _.internal
  let Tuple reducer outMsg = _stateTransition event internalState
  H.modify_ (\st -> st { internal = reducer st.internal })
  case outMsg of
    Just msg -> H.raise msg
    Nothing -> pure unit
  pure next

  where
    _stateTransition = stateTransition defaultConfig


buildComponent :: forall f a m. Foldable f => (a -> String) -> State f a -> H.Component HH.HTML Query InMsg OutMsg m
buildComponent li initialState = H.component spec
  where
    spec :: H.ComponentSpec HH.HTML (State f a) Query InMsg OutMsg m
    spec =
      { initialState : const initialState
      , render: buildRender li
      , eval
      , receiver : const Nothing
      }


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI (buildComponent show {internal : empty, external : Just [ 1, 2, 3 ] }) unit body
