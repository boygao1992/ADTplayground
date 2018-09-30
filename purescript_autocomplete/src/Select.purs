module Select where

import Prelude

import CSS.Geometry as CG
import CSS.Size as CS
import Control.MonadPlus (guard)
import Data.Array (take)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Int (toNumber, floor, ceil)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Utils (classList, (!))
import Web.DOM.Element as WDE
import Web.Event.Event as WEE
import Web.HTML.HTMLElement as WHHE
import Web.UIEvent.KeyboardEvent as KE

heightPx :: Int
heightPx = 30

type Config =
  { windowSize :: Int
  , listSize :: Int
  }

defaultConfig :: Config
defaultConfig =
  { windowSize : 5
  , listSize : 10
  }

type InternalState =
  { key :: Maybe Int
  , mouse :: Maybe Int
  , head :: Number
  }

empty :: InternalState
empty =
  { key : Nothing
  , mouse : Nothing
  , head : 0.0
  }

type State item =
  { config :: Config
  , internal :: InternalState
  , external :: Maybe (Array item)
  }

data Query item next
  = StateTransition Msg next
  | Sync (Array item) next
  | Configure Config next -- reconfigure at runtime
  -- | PreventDefault WEE.Event (Query f item next)
  | OnKeyDown KE.KeyboardEvent next
  | OnScroll next
  -- | Init next
  -- | HandleKey KE.KeyboardEvent (H.SubscribeStatus -> next)

data Msg
  = Keyboard KeyboardMsg
  | Scroll ScrollMsg
  | Mouse MouseMsg
  | Reset

type Input item = Array item

data Output
  = WindowPushUpperBoundary
  | WindowPushLowerBoundary
  | WindowSlideUp
  | WindowSlideDown
  | Select Int

type IO = Aff

stateTransition :: Config -> Msg -> InternalState -> Tuple (InternalState -> InternalState) (Maybe Output)
stateTransition config event state = case event of
  Keyboard subEvent ->
    keyboardTransition config subEvent state
  Scroll subEvent ->
    scrollTransition config subEvent state
  Mouse subEvent ->
    mouseTransition  config subEvent state
  Reset ->
    Tuple (const empty) Nothing

data KeyboardMsg
  = KeyUp
  | KeyDown

keyboardTransition
  :: forall r
   . Config ->
     KeyboardMsg ->
     { key :: Maybe Int, head :: Number | r } ->
     Tuple
       ({ key :: Maybe Int, head :: Number | r } ->
        { key :: Maybe Int, head :: Number | r }
       )
       (Maybe Output)
keyboardTransition { windowSize, listSize } event { key, head } =
  case event of
  KeyUp ->
    case key of
    Nothing ->
      (_ { key = Just ((floor head) + windowSize - 1) }) ! Just WindowSlideDown
    Just keyPos ->
      let
        windowAtUpperBoundary h =
          h <= 0
        aboveWindow k h =
          k - (ceil h) < 0
        belowWindow k h =
          k - (floor h) >= windowSize
        outsideWindow k h =
          k - (floor h) < 0 || k - (ceil h) > windowSize
      in
        if outsideWindow keyPos head
        then
          (_ { key = Just ((floor head) + windowSize - 1) }) ! Just WindowSlideDown
        else
          case windowAtUpperBoundary (floor head), aboveWindow (keyPos - 1) head of
          true, true  ->
            identity ! Just WindowPushUpperBoundary
          false, true ->
            (_ { key = Just $ keyPos - 1, head = toNumber $ keyPos - 1 }) ! Just WindowSlideUp
          _, _ ->
            (_ { key = Just $ keyPos - 1 }) ! Nothing

  KeyDown ->
    case key of
    Nothing ->
      (_ { key = Just $ floor $ head }) ! Just WindowSlideUp
    Just keyPos ->
      let
        windowAtLowerBoundary h =
          h >= (listSize - windowSize)
        aboveWindow k h =
          k - (ceil h) < 0
        belowWindow k h =
          k - (floor h) >= windowSize
        outsideWindow k h =
          k - (floor h) < 0 || k - (ceil h) > windowSize
      in
        if outsideWindow keyPos head
        then
          (_ { key = Just $ floor $ head }) ! Just WindowSlideUp
        else
          case windowAtLowerBoundary (floor head), belowWindow (keyPos + 1) head of
          true, true ->
            identity ! Just WindowPushLowerBoundary
          false, true ->
            (_ { key = Just $ keyPos + 1, head = toNumber $ keyPos + 2 - windowSize }) ! Just WindowSlideDown
          _, _ ->
            (_ { key = Just $ keyPos + 1 }) ! Nothing

data ScrollMsg
  = UpdateScrollPosition Number

scrollTransition
  :: forall r
   . Config ->
     ScrollMsg ->
     { head :: Number | r } ->
     Tuple
       ({ head :: Number | r } ->
        { head :: Number | r }
       )
       (Maybe Output)
scrollTransition { windowSize, listSize } event { head } =
  case event of
    UpdateScrollPosition h ->
      (_ { head = h }) ! Nothing


-- data ScrollMsg
--   = ScrollUp
--   | ScrollDown

-- scrollTransition
--   :: forall r
--    . Config ->
--      ScrollMsg ->
--      { key :: Maybe Int, head :: Int | r } ->
--      Tuple
--        ({ key :: Maybe Int, head :: Int | r } ->
--         { key :: Maybe Int, head :: Int | r }
--        )
--        (Maybe Output)
-- scrollTransition { windowSize, listSize } event { key, head } =
--   let
--     keyOutOfWindow k h =
--       k < h || k >= h + windowSize
--     updateKey :: Maybe Int -> Int -> Maybe Int
--     updateKey Nothing  _ = Nothing
--     updateKey (Just keyPos) h
--       | keyOutOfWindow keyPos h = Nothing
--       | otherwise = Just keyPos
--   in
--     case event of
--     ScrollUp ->
--       let
--         windowAtUpperBoundary h =
--           h == 0
--         Tuple newHead outMsg =
--           if windowAtUpperBoundary head then
--             Tuple head (Just WindowPushUpperBoundary)
--           else
--             Tuple (head - 1) Nothing
--       in
--         (_ { key = updateKey key newHead, head = newHead }) ! outMsg
--     ScrollDown ->
--       let
--         windowAtLowerBoundary h =
--           h == listSize - windowSize
--         Tuple newHead outMsg =
--           if windowAtLowerBoundary head then
--             Tuple head (Just WindowPushLowerBoundary)
--           else
--             Tuple (head + 1) Nothing
--       in
--         (_ { key = updateKey key newHead, head = newHead }) ! outMsg

data MouseMsg
  = MouseEnter Int
  | MouseLeave
  | MouseClick Int

mouseTransition
  :: forall r
   . Config ->
     MouseMsg ->
     { mouse :: Maybe Int | r } ->
     Tuple
       ({ mouse :: Maybe Int | r } ->
        { mouse :: Maybe Int | r }
       )
       (Maybe Output)
mouseTransition _ event state@{ mouse } =
  case event of
    MouseEnter mousePos ->
      (_ { mouse = Just mousePos }) ! Nothing
    MouseLeave ->
      (_ { mouse = Nothing }) ! Nothing
    MouseClick mousePos ->
      (_ { mouse = Just mousePos }) ! Just (Select mousePos)

ulRef :: H.RefLabel
ulRef = H.RefLabel "autocomplete-ul"

keySelectedRef :: H.RefLabel
keySelectedRef = H.RefLabel "keySelected"

buildRender :: forall item. (item -> String) -> State item -> H.ComponentHTML (Query item)
buildRender li { config, internal , external } =
  HH.div_
    [ HH.div_
        [ HH.text $ show internal ]
    , HH.div [ HP.class_ $ H.ClassName "example-autocomplete"]
        [ HH.input
          [ HE.onKeyDown $ HE.input OnKeyDown
          , HP.class_ $ H.ClassName "autocomplete-input"
          ]
        , HH.div [ HP.class_ $ H.ClassName "autocomplete-menu" ]
            [ toUl external ]
        ]
    , HH.div_
        [ HH.button [ HE.onClick $ HE.input_ $ StateTransition $ Keyboard KeyUp ]
            [ HH.text "KeyUp" ]
        , HH.button [ HE.onClick $ HE.input_ $ StateTransition $ Keyboard KeyDown ]
            [ HH.text "KeyDown" ]
        -- , HH.button [ HE.onClick $ HE.input_ $ StateTransition $ Scroll ScrollUp ]
        --     [ HH.text "ScrollUp" ]
        -- , HH.button [ HE.onClick $ HE.input_ $ StateTransition $ Scroll ScrollDown ]
        --     [ HH.text "ScrollDown" ]
        ]
    ]
  where
    toUl :: Maybe (Array item) -> H.ComponentHTML (Query item)
    toUl = case _ of
      Just xs  ->
        HH.ul [ HE.onScroll $ HE.input_ OnScroll
              , HP.class_ $ H.ClassName "autocomplete-list"
              , HC.style $ CG.maxHeight $ CS.px $ toNumber
                  $ heightPx * config.windowSize -- HACK: hard-coded offsetHeight
              , HP.ref ulRef
              ]
          $ foldlWithIndex (\index acc item -> acc <> [ toLi index item ]) [] xs
      _ ->
        HH.div_ []

    toLi :: Int -> item -> H.ComponentHTML (Query item)
    toLi index item =
      HH.li (join
              [ guard (Just index == internal.key)
                  $> HP.ref keySelectedRef
              ]
            <>
              [ classList
                  [ (Tuple "autocomplete-item" true)
                  , (Tuple "key-selected" (Just index == internal.key))
                  , (Tuple "mouse-selected" (Just index == internal.mouse))
                  ]
              ]
            <>
              [ HE.onMouseEnter $ HE.input_ $ StateTransition $ Mouse $ MouseEnter index
              , HE.onMouseLeave $ HE.input_ $ StateTransition $ Mouse $ MouseLeave
              , HE.onClick $ HE.input_ $ StateTransition $ Mouse $ MouseClick index
              ]
            )
        [ HH.text $ li item ]

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

eval :: forall item. Query item ~> H.ComponentDSL (State item) (Query item) Output IO
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
eval (Sync xs next) = do
  listSize <- H.gets _.config.listSize
  H.modify_ (\st -> st { internal = empty, external = Just $ take listSize xs })

  mul <- H.getHTMLElementRef ulRef
  case mul of
    Just ul -> do
      H.liftEffect
        $ WDE.setScrollTop 0.0 (WHHE.toElement ul)
    _ -> pure unit

  pure next
eval (Configure config next) = do
  H.modify_ (\st -> st { config = config })
  pure next
eval (StateTransition event next) = do
  config <- H.gets _.config
  internalState <- H.gets _.internal
  let Tuple reducer outMsg = _stateTransition config event internalState
  H.modify_ (\st -> st { internal = reducer st.internal })
  case outMsg of
    Just msg -> H.raise msg
    Nothing -> pure unit
  pure next

  where
    _stateTransition = stateTransition

eval (OnKeyDown ev next) = do
  case KE.key ev of
    "ArrowUp" -> do
      H.liftEffect <<< WEE.preventDefault <<< KE.toEvent $ ev
      config <- H.gets _.config
      internalState <- H.gets _.internal
      let Tuple reducer outMsg = _stateTransition config (Keyboard KeyUp) internalState
      H.modify_ (\st -> st { internal = reducer st.internal })
      case outMsg of
        Just WindowSlideUp -> do
          mul <- H.getHTMLElementRef ulRef
          mli <- H.getHTMLElementRef keySelectedRef
          case mul, mli of
            Just ul, Just li -> do
              liTop <- H.liftEffect $ WHHE.offsetTop li
              liHeight <- H.liftEffect $ WHHE.offsetHeight li
              H.liftEffect
                $ WDE.setScrollTop (liTop) (WHHE.toElement ul)
            _, _ -> pure unit
        _ -> do
          pure unit
      case outMsg of
        Just msg -> H.raise msg
        Nothing -> pure unit
      pure next
    "ArrowDown" -> do
      H.liftEffect <<< WEE.preventDefault <<< KE.toEvent $ ev
      config <- H.gets _.config
      internalState <- H.gets _.internal
      let Tuple reducer outMsg = _stateTransition config (Keyboard KeyDown) internalState
      H.modify_ (\st -> st { internal = reducer st.internal })
      case outMsg of
        Just WindowSlideDown -> do
          mul <- H.getHTMLElementRef ulRef
          mli <- H.getHTMLElementRef keySelectedRef
          case mul, mli of
            Just ul, Just li -> do
              liTop <- H.liftEffect $ WHHE.offsetTop li
              liHeight <- H.liftEffect $ WHHE.offsetHeight li
              ulHeight <- H.liftEffect $ WDE.clientHeight (WHHE.toElement ul)
              H.liftEffect
                $ WDE.setScrollTop
                    (liTop + liHeight - ulHeight)
                    (WHHE.toElement ul)
            _, _ -> pure unit
        _ -> do
          pure unit
      case outMsg of
        Just msg -> H.raise msg
        Nothing -> pure unit
      pure next
    _ -> pure next

  where
    _stateTransition = stateTransition

-- eval (PreventDefault ev next) = do
--   H.liftEffect $ WEE.preventDefault ev
--   eval next
eval (OnScroll next) = do
  mul <- H.getHTMLElementRef ulRef
  case mul of
    Just ul-> do
      ulHeight <- H.liftEffect $ WDE.scrollTop (WHHE.toElement ul)
      eval (StateTransition (Scroll $ UpdateScrollPosition $ ulHeight / (toNumber heightPx)) next)
    Nothing -> pure next



buildComponent :: forall item. (item -> String) -> State item -> H.Component HH.HTML (Query item) (Input item) Output IO
buildComponent li initialState = H.component spec
  where
    spec :: H.ComponentSpec HH.HTML (State item) (Query item) (Input item) Output IO
    spec =
      { initialState : const initialState
      , render: buildRender li
      , eval
      , receiver : HE.input Sync
      }

-- buildComponent :: forall f item. Foldable f => (item -> String) -> State f item -> H.Component HH.HTML Query Input Output IO
-- buildComponent li initialState = H.lifecycleComponent spec
--   where
--     spec :: H.LifecycleComponentSpec HH.HTML (State f item) Query Input Output IO
--     spec =
--       { initialState : const initialState
--       , render: buildRender li
--       , eval
--       , receiver : const Nothing
--       , initializer : Just (H.action Init)
--       , finalizer : Nothing
--       }


