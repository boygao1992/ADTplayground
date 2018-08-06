module WindowSlide1 exposing (..)

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


type alias WindowIndex =
    -- constraint: [0, windowSize)
    Int


type alias ListIndex =
    -- constraint: [0, listSize)
    Int



-- type alias KeycursorState =
--     Maybe WindowIndex


type alias State =
    { keycursor : Maybe WindowIndex
    , mousecursor : Maybe WindowIndex
    , head : ListIndex
    }


initialState : State
initialState =
    { keycursor = Maybe.Just 0
    , mousecursor = Maybe.Nothing
    , head = 0
    }


type InMsg
    = KeyUp
    | KeyDown
    | MouseChange WindowIndex
    | WindowSlideUp
    | WindowSlideDown


type OutMsg
    = WindowTooHigh
    | WindowTooLow
    | NoOp


update : Config -> InMsg -> State -> ( State, OutMsg )
update config msg state =
    case msg of
        KeyUp ->
            keycursorUpdate config msg state

        KeyDown ->
            keycursorUpdate config msg state

        _ ->
            ( state, NoOp )


keycursorUpdate : Config -> InMsg -> State -> ( State, OutMsg )
keycursorUpdate config msg state =
    let
        keycursor =
            state.keycursor

        aboveWindow index =
            if index < 0 then
                True
            else
                False

        belowWindow index =
            if (index >= config.windowSize) then
                True
            else
                False

        keycursorUp index =
            if aboveWindow <| index - 1 then
                let
                    ( newState, newMsg ) =
                        update config WindowSlideUp state
                in
                    case newMsg of
                        WindowTooHigh ->
                            index

                        _ ->
                            index - 1
            else
                index - 1

        keycursorDown index =
            if belowWindow <| index + 1 then
                let
                    ( newState, newMsg ) =
                        update config WindowSlideDown state
                in
                    case newMsg of
                        WindowTooLow ->
                            index

                        _ ->
                            index + 1
            else
                index + 1
    in
        case msg of
            KeyUp ->
                ( { state
                    | keycursor =
                        keycursor |> Maybe.map keycursorUp
                  }
                , NoOp
                )

            KeyDown ->
                ( { state
                    | keycursor =
                        keycursor
                            |> Maybe.map keycursorDown
                  }
                , NoOp
                )

            _ ->
                ( state, NoOp )


view : State -> H.Html InMsg
view state =
    H.div []
        [ H.div [] [ H.text << toString <| state ]
        , H.div []
            [ H.button
                [ E.onClick KeyUp ]
                [ H.text "KeyUp" ]
            , H.button
                [ E.onClick KeyDown ]
                [ H.text "KeyDown" ]
            ]
        , H.div []
            [ H.button
                [ E.onClick WindowSlideUp ]
                [ H.text "WindowSlideUp" ]
            , H.button
                [ E.onClick WindowSlideDown ]
                [ H.text "WindowSlideDown" ]
            ]
        ]


main : Program Never State InMsg
main =
    H.program
        { init = ( initialState, Cmd.none )
        , update =
            \inMsg state ->
                update defaultConfig inMsg state
                    |> \( newState, _ ) -> ( newState, Cmd.none )
        , subscriptions = \model -> Sub.none
        , view = view
        }
