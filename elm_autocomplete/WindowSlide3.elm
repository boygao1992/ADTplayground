module WindowSlide3 exposing (..)

import Html as H
import Html.Events as E


-- import Html.Attributes as A


type alias Config =
    { windowSize : Int
    , listSize : Int
    }


defaultConfig : Config
defaultConfig =
    { windowSize = 3
    , listSize = 10
    }


type alias State =
    { keycursor : Maybe Int
    , mousecursor : Maybe Int
    , head : Int
    }


setKeycursor :
    Maybe Int
    -> { a | keycursor : Maybe Int }
    -> { a | keycursor : Maybe Int }
setKeycursor k s =
    { s | keycursor = k }


setHead :
    Int
    -> { a | head : Int }
    -> { a | head : Int }
setHead h s =
    { s | head = h }


initialState : State
initialState =
    { keycursor = Maybe.Just 0
    , mousecursor = Maybe.Nothing
    , head = 0
    }


type InMsg
    = Keyboard KeyboardMsg
    | Scroll ScrollMsg
    | Mouse MouseMsg


type KeyboardMsg
    = KeyUp
    | KeyDown


type ScrollMsg
    = ScrollUp
    | ScrollDown


type MouseMsg
    = MouseEnter Int
    | MouseLeave
    | MouseClick


type OutMsg
    = WindowPushUpperBoundary
    | WindowPushLowerBoundary
    | NoOp


stateTransition : Config -> InMsg -> State -> ( State, OutMsg )
stateTransition config msg state =
    case msg of
        Keyboard keyboardMsg ->
            keyboardTransition config keyboardMsg state

        Scroll scrollMsg ->
            scrollTransition config scrollMsg state

        _ ->
            ( state, NoOp )


keyboardTransition :
    Config
    -> KeyboardMsg
    -> { a | keycursor : Maybe Int, head : Int }
    -> ( { a | keycursor : Maybe Int, head : Int }, OutMsg )
keyboardTransition config msg state =
    let
        { keycursor, head } =
            state
    in
        case msg of
            KeyUp ->
                case keycursor of
                    Maybe.Nothing ->
                        ( state
                            |> setKeycursor (Maybe.Just (head + config.windowSize - 1))
                        , NoOp
                        )

                    Maybe.Just key ->
                        let
                            aboveWindow k h =
                                k - h < 0

                            windowAtUpperBoundary h =
                                h == 0
                        in
                            case ( windowAtUpperBoundary head, aboveWindow (key - 1) head ) of
                                ( True, True ) ->
                                    ( state, WindowPushUpperBoundary )

                                ( True, False ) ->
                                    ( state
                                        |> setKeycursor (Maybe.Just (key - 1))
                                    , NoOp
                                    )

                                ( False, True ) ->
                                    ( state
                                        |> setKeycursor (Maybe.Just (key - 1))
                                        >> setHead (head - 1)
                                    , NoOp
                                    )

                                ( False, False ) ->
                                    ( state
                                        |> setKeycursor (Maybe.Just (key - 1))
                                    , NoOp
                                    )

            KeyDown ->
                case keycursor of
                    Maybe.Nothing ->
                        ( state
                            |> setKeycursor (Maybe.Just head)
                        , NoOp
                        )

                    Maybe.Just key ->
                        let
                            belowWindow k h =
                                k - h >= config.windowSize

                            windowAtLowerBoundary h =
                                h == config.listSize - config.windowSize
                        in
                            case ( windowAtLowerBoundary head, belowWindow (key + 1) head ) of
                                ( True, True ) ->
                                    ( state, WindowPushLowerBoundary )

                                ( True, False ) ->
                                    ( state
                                        |> setKeycursor (Maybe.Just (key + 1))
                                    , NoOp
                                    )

                                ( False, True ) ->
                                    ( state
                                        |> setKeycursor (Maybe.Just (key + 1))
                                        >> setHead (head + 1)
                                    , NoOp
                                    )

                                ( False, False ) ->
                                    ( state
                                        |> setKeycursor (Maybe.Just (key + 1))
                                    , NoOp
                                    )


scrollTransition :
    Config
    -> ScrollMsg
    -> { a | keycursor : Maybe Int, head : Int }
    -> ( { a | keycursor : Maybe Int, head : Int }, OutMsg )
scrollTransition config msg state =
    let
        { keycursor, head } =
            state

        keycursorOutOfWindow k h =
            k < h || k >= h + config.windowSize

        keycursorUpdate k h =
            case k of
                Maybe.Nothing ->
                    Maybe.Nothing

                Maybe.Just key ->
                    if keycursorOutOfWindow key h then
                        Maybe.Nothing
                    else
                        Maybe.Just key
    in
        case msg of
            ScrollUp ->
                let
                    windowAtUpperBoundary h =
                        h == 0

                    ( new_head, outmsg ) =
                        if windowAtUpperBoundary head then
                            ( head, WindowPushUpperBoundary )
                        else
                            ( head - 1, NoOp )

                    new_keycursor =
                        keycursorUpdate keycursor new_head
                in
                    ( state
                        |> setKeycursor new_keycursor
                        >> setHead new_head
                    , outmsg
                    )

            ScrollDown ->
                let
                    windowAtLowerBoundary h =
                        h == config.listSize - config.windowSize

                    ( new_head, outmsg ) =
                        if windowAtLowerBoundary head then
                            ( head, WindowPushLowerBoundary )
                        else
                            ( head + 1, NoOp )

                    new_keycursor =
                        keycursorUpdate keycursor new_head
                in
                    ( state
                        |> setKeycursor new_keycursor
                        >> setHead new_head
                    , outmsg
                    )


update : InMsg -> State -> ( State, Cmd msg )
update inMsg state =
    stateTransition defaultConfig inMsg state
        |> \( newState, _ ) -> ( newState, Cmd.none )



-- viewWithCustomizedMsg :
--     { keyUp : msg
--     , keyDown : msg
--     , scrollUp : msg
--     , scrollDown : msg
--     }
--     -> State
--     -> H.Html msg
-- viewWithCustomizedMsg { keyUp, keyDown, scrollUp, scrollDown } state =
--     H.div []
--         [ H.div [] [ H.text << toString <| state ]
--         , H.div
--             []
--             [ H.button
--                 [ E.onClick keyUp ]
--                 [ H.text "KeyUp" ]
--             , H.button
--                 [ E.onClick keyDown ]
--                 [ H.text "KeyDown" ]
--             ]
--         , H.div
--             []
--             [ H.button
--                 [ E.onClick scrollUp ]
--                 [ H.text "WindowSlideUp" ]
--             , H.button
--                 [ E.onClick scrollDown ]
--                 [ H.text "WindowSlideDown" ]
--             ]
--         ]
-- view : State -> H.Html InMsg
-- view state =
--     let
--         config =
--             { keyUp = Keyboard KeyUp
--             , keyDown = Keyboard KeyDown
--             , scrollUp = Scroll ScrollUp
--             , scrollDown = Scroll ScrollDown
--             }
--     in
--         viewWithCustomizedMsg config state


view : State -> H.Html InMsg
view state =
    H.div []
        [ H.div [] [ H.text << toString <| state ]
        , H.div
            []
            [ H.button
                [ E.onClick <| Keyboard KeyUp ]
                [ H.text "KeyUp" ]
            , H.button
                [ E.onClick <| Keyboard KeyDown ]
                [ H.text "KeyDown" ]
            ]
        , H.div
            []
            [ H.button
                [ E.onClick <| Scroll ScrollUp ]
                [ H.text "WindowSlideUp" ]
            , H.button
                [ E.onClick <| Scroll ScrollDown ]
                [ H.text "WindowSlideDown" ]
            ]
        ]


main : Program Never State InMsg
main =
    H.program
        { init = ( initialState, Cmd.none )
        , update = update
        , subscriptions = \model -> Sub.none
        , view = view
        }
