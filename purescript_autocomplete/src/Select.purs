module Select where

import Prelude

import CSS.Geometry as CG
import CSS.Size as CS
import Control.MonadPlus (guard)
import Data.Array (filter, index, take, find) as A
-- import Data.Foldable (traverse_)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Int (toNumber, floor, ceil)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor.Strong ((&&&))
import Data.Set (fromFoldable, member) as Set
import Data.String (Pattern(Pattern), contains) as S
import Data.Tuple (Tuple(..))
import Data.Tuple (fst, snd) as Tuple
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

type DataConfig item q =
  { toHTML :: item -> H.ComponentHTML q
  , toId :: item -> String
  , toKeyword :: item -> String
  }

defaultConfig :: Config
defaultConfig =
  { windowSize : 5
  , listSize : 10
  }

type Index = Int
type Id = String
type Content = String

type InternalState =
  { key :: Maybe Int
  , mouse :: Maybe Int
  , head :: Number
  , selection :: Maybe Index
  , search :: String
  , candidates :: Maybe (Array Id)
  }

empty :: InternalState
empty =
  { key : Nothing
  , mouse : Nothing
  , head : 0.0
  , selection : Nothing
  , search : ""
  , candidates : Nothing
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
  | OnKeyDown KE.KeyboardEvent next
  | OnScroll next
  | OnInput String next
  -- | Init next
  -- | HandleKey KE.KeyboardEvent (H.SubscribeStatus -> next)

data Msg
  = Keyboard KeyboardMsg
  | Scroll ScrollMsg
  | Mouse MouseMsg
  | Search SearchMsg
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
  Search subEvent ->
    searchTransition config subEvent state
  Reset ->
    Tuple (const empty) Nothing

data SearchMsg
  = SearchInput String (Array (Tuple Id Content))

searchTransition
  :: forall r
   . Config
  -> SearchMsg
  -> { search :: String, candidates :: Maybe(Array Id) | r }
  -> Tuple
       ({ search :: String, candidates :: Maybe(Array Id) | r } ->
        { search :: String, candidates :: Maybe(Array Id) | r }
       )
       (Maybe Output)
searchTransition { listSize } event _ = case event of
  SearchInput search xs ->
    let
      candidates
          = A.take listSize
        <<< map Tuple.fst
        <<< A.filter ( S.contains (S.Pattern search) <<< Tuple.snd)
          $ xs
    in
     (_ { search = search, candidates = Just candidates }) ! Nothing


data KeyboardMsg
  = KeyUp
  | KeyDown

keyboardTransition
  :: forall r
   . Config ->
     KeyboardMsg ->
     { key :: Maybe Int, mouse :: Maybe Int, head :: Number, selection :: Maybe Index | r } ->
     Tuple
       ({ key :: Maybe Int, mouse :: Maybe Int, head :: Number, selection :: Maybe Index | r } ->
        { key :: Maybe Int, mouse :: Maybe Int, head :: Number, selection :: Maybe Index | r }
       )
       (Maybe Output)
keyboardTransition { windowSize, listSize } event { key, mouse, head } =
  case event of
  KeyUp ->
    case key of
    Nothing ->
      let
        selection = Just ((floor head) + windowSize - 1)
      in
       case mouse of
         Just _ ->
           (_ { key = selection } ) ! Just WindowSlideDown
         Nothing ->
           (_ { key = selection, selection = selection } ) ! Just WindowSlideDown
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
          let
            selection = Just ((floor head) + windowSize - 1)
          in
            case mouse of
              Just _ ->
                (_ { key = selection }) ! Just WindowSlideDown
              Nothing ->
                (_ { key = selection , selection = selection }) ! Just WindowSlideDown
        else
          case windowAtUpperBoundary (floor head), aboveWindow (keyPos - 1) head of
          true, true  ->
            identity ! Just WindowPushUpperBoundary
          false, true ->
            let
              selection = Just $ keyPos - 1
            in
              case mouse of
                Just _ ->
                  (_ { key = selection, head = toNumber $ keyPos - 1 }) ! Just WindowSlideUp
                Nothing ->
                  (_ { key = selection, head = toNumber $ keyPos - 1, selection = selection }) ! Just WindowSlideUp
          _, _ ->
            let
              selection = Just $ keyPos - 1
            in
              case mouse of
                Just _ ->
                  (_ { key = selection}) ! Nothing
                Nothing ->
                  (_ { key = selection, selection = selection}) ! Nothing

  KeyDown ->
    case key of
    Nothing ->
      let
        selection = Just $ floor $ head
      in
        case mouse of
          Just _ ->
            (_ { key = selection }) ! Just WindowSlideUp
          Nothing ->
            (_ { key = selection, selection = selection }) ! Just WindowSlideUp
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
          let
            selection = Just $ floor $ head
          in
            case mouse of
              Just _ ->
                (_ { key = selection }) ! Just WindowSlideUp
              Nothing ->
                (_ { key = selection, selection = selection }) ! Just WindowSlideUp
        else
          case windowAtLowerBoundary (floor head), belowWindow (keyPos + 1) head of
          true, true ->
            identity ! Just WindowPushLowerBoundary
          false, true ->
            let
              selection = Just $ keyPos + 1
            in
              case mouse of
                Just _ ->
                  (_ { key = selection, head = toNumber $ keyPos + 2 - windowSize }) ! Just WindowSlideDown
                Nothing ->
                  (_ { key = selection, head = toNumber $ keyPos + 2 - windowSize, selection = selection }) ! Just WindowSlideDown
          _, _ ->
            let
              selection = Just $ keyPos + 1
            in
              case mouse of
                Just _ ->
                  (_ { key = selection }) ! Nothing
                Nothing ->
                  (_ { key = selection, selection = selection }) ! Nothing

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
     { key :: Maybe Int, mouse :: Maybe Int, selection :: Maybe Int | r } ->
     Tuple
       ({ key :: Maybe Int, mouse :: Maybe Int, selection :: Maybe Int | r } ->
        { key :: Maybe Int, mouse :: Maybe Int, selection :: Maybe Int | r }
       )
       (Maybe Output)
mouseTransition _ event state@{ key, mouse } =
  case event of
    MouseEnter mousePos ->
      let
        selection = Just mousePos
      in
       (_ { mouse = selection, selection = selection }) ! Nothing
    MouseLeave ->
      (_ { mouse = Nothing, selection = key }) ! Nothing
    MouseClick mousePos ->
      let
        selection = Just mousePos
      in
        (_ { mouse = selection, selection = selection }) ! Just (Select mousePos)

ulRef :: H.RefLabel
ulRef = H.RefLabel "autocomplete-ul"

keySelectedRef :: H.RefLabel
keySelectedRef = H.RefLabel "keySelected"

inputRef :: H.RefLabel
inputRef = H.RefLabel "autocomplete-input"

buildRender
  :: forall item
   . DataConfig item (Query item)
  -> State item
  -> H.ComponentHTML (Query item)
buildRender { toHTML, toId, toKeyword } { config, internal , external } =
  HH.div_
    [ HH.div [ HP.class_ $ H.ClassName "example-autocomplete"]
        [ HH.input
          [ HE.onKeyDown $ HE.input OnKeyDown
          , HE.onValueInput $ HE.input OnInput
          , HP.class_ $ H.ClassName "autocomplete-input"
          , HP.value $ inputValue
          , HP.ref inputRef
          ]
        , HH.div [ HP.class_ $ H.ClassName "autocomplete-menu" ]
            [ toUl candidateList ]
        ]
    , HH.div_
        [ HH.text $ show internal ]
    ]
  where
    candidateList :: Array item
    candidateList =
      let
        xs = fromMaybe [] $ external
      in
        case internal.candidates of
          Just ids ->
            let
              idSet = Set.fromFoldable ids
            in
              A.filter (flap(Set.member) idSet <<< toId) xs
          Nothing ->
            xs

    inputValue :: String
    inputValue = fromMaybe internal.search do
      items <- external
      idx <- internal.selection
      ids <- internal.candidates
      id <- A.index ids idx
      item <- A.find ( (_ == id) <<< toId ) items
      pure $ toKeyword item

    toUl :: Array item -> H.ComponentHTML (Query item)
    toUl xs =
      HH.ul [ HE.onScroll $ HE.input_ OnScroll
            , HP.class_ $ H.ClassName "autocomplete-list"
            , HC.style $ CG.maxHeight $ CS.px $ toNumber
                $ heightPx * config.windowSize -- HACK: hard-coded offsetHeight
            , HP.ref ulRef
            ]
        $ foldlWithIndex (\index acc item -> acc <> [ toLi index item ]) [] xs

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
        [ toHTML item ]

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

buildEval
  :: forall item
   . DataConfig item (Query item)
  -> Query item
  ~> H.ComponentDSL (State item) (Query item) Output IO
buildEval { toId, toKeyword } = eval
  where
    eval :: Query item ~> H.ComponentDSL (State item) (Query item) Output IO
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
      { config , internal : internalState } <- H.get
      H.modify_ (\st -> st { internal = st.internal { key = Nothing, mouse = Nothing, head = 0.0, selection = Nothing } , external = Just $ xs })
      let Tuple reducer outMsg = stateTransition config (Search (SearchInput internalState.search (( toId &&& toKeyword) <$> xs))) internalState
      H.modify_ (\st -> st { internal = reducer st.internal })

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
      { config, internal : internalState } <- H.get
      let Tuple reducer outMsg = stateTransition config event internalState
      H.modify_ (\st -> st { internal = reducer st.internal })
      case outMsg of
        Just msg -> H.raise msg
        Nothing -> pure unit
      pure next


    eval (OnKeyDown ev next) = do
      case KE.key ev of
        "ArrowUp" -> do
          H.liftEffect <<< WEE.preventDefault <<< KE.toEvent $ ev
          config <- H.gets _.config
          internalState <- H.gets _.internal
          let Tuple reducer outMsg = stateTransition config (Keyboard KeyUp) internalState
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
          let Tuple reducer outMsg = stateTransition config (Keyboard KeyDown) internalState
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

    eval (OnScroll next) = do
      mul <- H.getHTMLElementRef ulRef
      case mul of
        Just ul-> do
          ulHeight <- H.liftEffect $ WDE.scrollTop (WHHE.toElement ul)
          eval (StateTransition (Scroll $ UpdateScrollPosition $ ulHeight / (toNumber heightPx)) next)
        Nothing -> pure next

    eval (OnInput search next) = do
      { config , internal : internalState, external } <- H.get
      let Tuple reducer outMsg = stateTransition config (Search (SearchInput search ((toId &&& toKeyword) <$> (fromMaybe [] external)))) internalState
      H.modify_ (\st -> st { internal = reducer st.internal })
      H.modify_ (\st -> st { internal = st.internal { key = Nothing, selection = Nothing } })
      pure next


buildComponent :: forall item. DataConfig item (Query item)-> State item -> H.Component HH.HTML (Query item) (Input item) Output IO
buildComponent dataConfig initialState = H.component spec
  where
    spec :: H.ComponentSpec HH.HTML (State item) (Query item) (Input item) Output IO
    spec =
      { initialState : const initialState
      , render: buildRender dataConfig
      , eval: buildEval dataConfig
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


