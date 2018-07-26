module WindowSlide1 exposing (..)


type alias Config =
    { windowSize : Int
    , listSize : Int
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
