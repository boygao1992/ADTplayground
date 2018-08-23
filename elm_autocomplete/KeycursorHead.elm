module KeycursorHead exposing (..)


type alias Config a =
    { a
        | windowSize : Int
        , listSize : Int
    }


type Head
    = HeadCeiling
    | HeadBottom
    | HeadMiddle Int


type Keycursor
    = KeyCeiling
    | KeyBottom
    | KeyMiddle Int
    | KeyNothing


type alias State =
    { keycursor : Keycursor
    , head : Head
    }


type OutMsg
    = WindowPushUpperBoundary
    | WindowPushLowerBoundary


keyup : Config a -> State -> ( State, Maybe OutMsg )
keyup config state =
    let
        { keycursor, head } =
            state

        keyMiddle current =
            let
                next =
                    current - 1
            in
                if (next == 0) then
                    ( { state | keycursor = KeyCeiling }, Nothing )
                else
                    ( { state | keycursor = KeyMiddle next }, Nothing )

        keyBottom =
            ( { state | keycursor = KeyMiddle (config.windowSize - 1) }, Nothing )
    in
        case ( keycursor, head ) of
            ( KeyNothing, _ ) ->
                ( { state | keycursor = KeyBottom }, Nothing )

            ( KeyCeiling, HeadCeiling ) ->
                ( state, Just WindowPushUpperBoundary )

            ( KeyBottom, HeadCeiling ) ->
                keyBottom

            ( KeyMiddle current, HeadCeiling ) ->
                keyMiddle current

            ( KeyCeiling, HeadBottom ) ->
                ( { state | head = HeadMiddle (config.listSize - config.windowSize - 1) }, Nothing )

            ( KeyBottom, HeadBottom ) ->
                keyBottom

            ( KeyMiddle current, HeadBottom ) ->
                keyMiddle current

            ( KeyCeiling, HeadMiddle current ) ->
                let
                    next =
                        current - 1
                in
                    if (next == 0) then
                        ( { state | head = HeadCeiling }, Nothing )
                    else
                        ( { state | head = HeadMiddle next }, Nothing )

            ( KeyBottom, HeadMiddle _ ) ->
                keyBottom

            ( KeyMiddle current, HeadMiddle _ ) ->
                keyMiddle current


keydown : Config a -> State -> ( State, Maybe OutMsg )
keydown config state =
    let
        { keycursor, head } =
            state

        keyMiddle current =
            let
                next =
                    current + 1
            in
                if (next == config.windowSize) then
                    ( { state | keycursor = KeyBottom }, Nothing )
                else
                    ( { state | keycursor = KeyMiddle next }, Nothing )

        keyCeiling =
            ( { state | keycursor = KeyMiddle 1 }, Nothing )
    in
        case ( keycursor, head ) of
            ( KeyNothing, _ ) ->
                ( { state | keycursor = KeyCeiling }, Nothing )

            ( KeyCeiling, HeadCeiling ) ->
                keyCeiling

            ( KeyBottom, HeadCeiling ) ->
                ( { state | head = HeadMiddle 1 }, Nothing )

            ( KeyMiddle current, HeadCeiling ) ->
                keyMiddle current

            ( KeyCeiling, HeadBottom ) ->
                keyCeiling

            ( KeyBottom, HeadBottom ) ->
                ( state, Just WindowPushLowerBoundary )

            ( KeyMiddle current, HeadBottom ) ->
                keyMiddle current

            ( KeyCeiling, HeadMiddle _ ) ->
                keyCeiling

            ( KeyBottom, HeadMiddle current ) ->
                let
                    next =
                        current - 1
                in
                    if (next == 0) then
                        ( { state | head = HeadCeiling }, Nothing )
                    else
                        ( { state | head = HeadMiddle next }, Nothing )

            ( KeyMiddle current, HeadMiddle _ ) ->
                keyMiddle current



-- scrollUp :: Config a  -> State -> State
-- scrollUp config state =
--     let
--         {keycursor, head} = state
--     in
--         case (keycursor, head) of
