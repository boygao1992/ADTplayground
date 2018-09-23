module Select where

import Prelude

import CSS.Geometry as CG
import CSS.Size as CS
import Control.MonadPlus (guard)
import Data.Foldable (class Foldable, foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Console (log, logShow)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Utils (classList)
import Web.DOM.Element as WDE
import Web.Event.Event as WEE
import Web.HTML.HTMLElement as WHHE
import Web.UIEvent.KeyboardEvent as KE

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
  { windowSize : 5
  , listSize : 10
  }

type InternalState =
  { key :: Maybe Int
  , mouse :: Maybe Int
  , head :: Int
  }

empty :: InternalState
empty =
  { key : Nothing
  , mouse : Nothing
  , head : 0
  }

type State item =
  { config :: Config
  , internal :: InternalState
  , external :: Maybe (Array item)
  }

data Query f item next
  = StateTransition Msg next
  | Sync (f item) next
  | Configure Config next -- reconfigure at runtime
  -- | PreventDefault WEE.Event (Query f item next)
  | OnKeyDown KE.KeyboardEvent next
  -- | OnScroll next
  -- | Init next
  -- | HandleKey KE.KeyboardEvent (H.SubscribeStatus -> next)

data Msg
  = Keyboard KeyboardMsg
  -- | Scroll ScrollMsg
  | Mouse MouseMsg
  | Reset

type Input f item = f item

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
  -- Scroll subEvent ->
  --   scrollTransition config subEvent state
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
     { key :: Maybe Int, head :: Int | r } ->
     Tuple
       ({ key :: Maybe Int, head :: Int | r } ->
        { key :: Maybe Int, head :: Int | r }
       )
       (Maybe Output)
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
        case windowAtUpperBoundary head, aboveWindow (keyPos - 1) head of
        true, true  ->
          identity ! Just WindowPushUpperBoundary
        true, false ->
          (_ { key = Just $ keyPos - 1 }) ! Nothing
        false, true ->
          (_ { key = Just $ keyPos - 1, head = head - 1 }) ! Just WindowSlideUp
        false, false ->
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
        case windowAtLowerBoundary head, belowWindow (keyPos + 1) head of
        true, true ->
          identity ! Just WindowPushLowerBoundary
        true, false ->
          (_ { key = Just $ keyPos + 1 }) ! Nothing
        false, true ->
          (_ { key = Just $ keyPos + 1, head = head + 1 }) ! Just WindowSlideDown
        false, false ->
          (_ { key = Just $ keyPos + 1 }) ! Nothing

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

buildRender :: forall f item. (item -> String) -> State item -> H.ComponentHTML (Query f item)
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
    toUl :: Maybe (Array item) -> H.ComponentHTML (Query f item)
    toUl e
      | Just x <- e =
          HH.ul [ -- HE.onScroll $ HE.input_ OnScroll,
                  HP.class_ $ H.ClassName "autocomplete-list"
                , HC.style $ CG.maxHeight $ CS.px $ toNumber
                    $ 30 * config.windowSize -- HACK: hard-coded offsetHeight
                , HP.ref ulRef
                ]
            $ foldlWithIndex (\index acc item -> acc <> [ toLi index item ]) [] x
      | otherwise = HH.div_ []

    toLi :: Int -> item -> forall f. H.ComponentHTML f
    toLi index item =
      HH.li (join
               [ pure $ classList
                   [ (Tuple "autocomplete-item" true)
                   , (Tuple "key-selected" (Just index == internal.key))
                   ]
               , guard (Just index == internal.key)
                   $> HP.ref keySelectedRef
               ])
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

eval :: forall f item. Foldable f => Query f item ~> H.ComponentDSL (State item) (Query f item) Output IO
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
  H.modify_ (\st -> st { external = Just $ foldl (\acc x -> acc <> [x]) [] xs })
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
-- eval (OnScroll next) = do
--   mhl<- H.getHTMLElementRef keySelectedRef
--   case mhl of
--     Just hl -> do
--       itemTop <- H.liftEffect $ WHHE.offsetTop hl
--       H.liftEffect $ logShow $ itemTop
--       itemHeight <- H.liftEffect $ WHHE.offsetHeight hl
--       H.liftEffect $ logShow $ itemHeight
--     Nothing -> pure unit
--   pure next



buildComponent :: forall f item. Foldable f => (item -> String) -> State item -> H.Component HH.HTML (Query f item) (Input f item) Output IO
buildComponent li initialState = H.component spec
  where
    spec :: H.ComponentSpec HH.HTML (State item) (Query f item) (Input f item) Output IO
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


