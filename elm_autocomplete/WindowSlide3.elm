module WindowSlide3 exposing (..)

import Html as H
import Html.Attributes as A
import Html.Events as E


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
    = MouseChange Int



-- type InMsg
--     = KeyUp
--     | KeyDown
--     | MouseChange WindowIndex
--     | WindowSlideUp
--     | WindowSlideDown


type OutMsg
    = WindowPushUpperBoundary
    | WindowPushLowerBoundary
    | NoOp


stateTransition : Config -> InMsg -> State -> ( State, OutMsg )
stateTransition config msg state =
    case msg of
        Keyboard keyboardMsg ->
            let
                { keycursor, head } =
                    state

                ( new_keycursor, new_head, outmsg ) =
                    keyboardTransition config keyboardMsg keycursor head
            in
                ( { state | keycursor = new_keycursor, head = new_head }, outmsg )

        Scroll scrollMsg ->
            let
                { keycursor, head } =
                    state

                ( new_keycursor, new_head, outmsg ) =
                    scrollTransition config scrollMsg keycursor head
            in
                ( { state | keycursor = new_keycursor, head = new_head }, outmsg )

        _ ->
            ( state, NoOp )


keyboardTransition : Config -> KeyboardMsg -> Maybe Int -> Int -> ( Maybe Int, Int, OutMsg )
keyboardTransition config msg keycursor head =
    case msg of
        KeyUp ->
            case keycursor of
                Maybe.Nothing ->
                    ( Maybe.Just (head + config.windowSize - 1), head, NoOp )

                Maybe.Just key ->
                    let
                        aboveWindow k h =
                            k - h < 0

                        windowAtUpperBoundary h =
                            h == 0
                    in
                        case ( windowAtUpperBoundary head, aboveWindow (key - 1) head ) of
                            ( True, True ) ->
                                ( Maybe.Just key, head, WindowPushUpperBoundary )

                            ( True, False ) ->
                                ( Maybe.Just (key - 1), head, NoOp )

                            ( False, True ) ->
                                ( Maybe.Just (key - 1), head - 1, NoOp )

                            ( False, False ) ->
                                ( Maybe.Just (key - 1), head, NoOp )

        KeyDown ->
            case keycursor of
                Maybe.Nothing ->
                    ( Maybe.Just head, head, NoOp )

                Maybe.Just key ->
                    let
                        belowWindow k h =
                            k - h >= config.windowSize

                        windowAtLowerBoundary h =
                            h == config.listSize - config.windowSize
                    in
                        case ( windowAtLowerBoundary head, belowWindow (key + 1) head ) of
                            ( True, True ) ->
                                ( Maybe.Just key, head, WindowPushLowerBoundary )

                            ( True, False ) ->
                                ( Maybe.Just (key + 1), head, NoOp )

                            ( False, True ) ->
                                ( Maybe.Just (key + 1), head + 1, NoOp )

                            ( False, False ) ->
                                ( Maybe.Just (key + 1), head, NoOp )


scrollTransition : Config -> ScrollMsg -> Maybe Int -> Int -> ( Maybe Int, Int, OutMsg )
scrollTransition config msg keycursor head =
    let
        keycursorOutOfWindow k h =
            k < h || k >= h + config.windowSize
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
                        case keycursor of
                            Maybe.Nothing ->
                                Maybe.Nothing

                            Maybe.Just key ->
                                if keycursorOutOfWindow key new_head then
                                    Maybe.Nothing
                                else
                                    Maybe.Just key
                in
                    ( new_keycursor, new_head, outmsg )

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
                        case keycursor of
                            Maybe.Nothing ->
                                Maybe.Nothing

                            Maybe.Just key ->
                                if keycursorOutOfWindow key new_head then
                                    Maybe.Nothing
                                else
                                    Maybe.Just key
                in
                    ( new_keycursor, new_head, outmsg )


update : InMsg -> State -> ( State, Cmd msg )
update inMsg state =
    stateTransition defaultConfig inMsg state
        |> \( newState, _ ) -> ( newState, Cmd.none )


view : State -> H.Html InMsg
view state =
    H.div []
        [ H.div [] [ H.text << toString <| state ]
        , H.map Keyboard <|
            H.div []
                [ H.button
                    [ E.onClick KeyUp ]
                    [ H.text "KeyUp" ]
                , H.button
                    [ E.onClick KeyDown ]
                    [ H.text "KeyDown" ]
                ]
        , H.map Scroll <|
            H.div []
                [ H.button
                    [ E.onClick ScrollUp ]
                    [ H.text "WindowSlideUp" ]
                , H.button
                    [ E.onClick ScrollDown ]
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
