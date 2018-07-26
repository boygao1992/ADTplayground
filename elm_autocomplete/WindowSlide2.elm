module WindowSlide2 exposing (..)


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


type Msg
    = KeyUp
    | KeyDown
    | MouseChange WindowIndex
    | WindowSlideUp
    | WindowSlideDown
    | WindowTooHigh
    | WindowTooLow
    | NoOp


update : Config -> Msg -> State -> ( State, Msg )
update config msg state =
    case msg of
        KeyUp ->
            keycursorUpdate config msg state

        KeyDown ->
            keycursorUpdate config msg state

        _ ->
            ( state, NoOp )



-- regulator : Config -> Msg -> State -> ( State, Msg )
-- regulator config msg state =


keycursorUpdate : Config -> Msg -> State -> ( State, Msg )
keycursorUpdate config msg state =
    let
        keycursor =
            state.keycursor
    in
        case msg of
            KeyUp ->
                ( state, NoOp )

            KeyDown ->
                ( state, NoOp )

            _ ->
                ( state, NoOp )
