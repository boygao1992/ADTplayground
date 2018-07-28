module Main exposing (..)

import Automaton exposing (..)
import Html as H


robot1 : Automaton number number
robot1 =
    pure ((+) 1)


type CounterMsg
    = Inc
    | Dec


robot2 : number -> Automaton CounterMsg number
robot2 init =
    let
        reducer event state =
            case event of
                Inc ->
                    state + 1

                Dec ->
                    state - 1
    in
        state init reducer


robot3 : Automaton CounterMsg String
robot3 =
    let
        reducer event state =
            case event of
                Inc ->
                    ( "increment by 1, now: " ++ toString (state + 1), state + 1 )

                Dec ->
                    ( "decrement by 1, now: " ++ toString (state - 1), state - 1 )
    in
        hiddenState 0 reducer


view : String -> H.Html msg
view str =
    H.text str


main : H.Html msg
main =
    -- view <| toString <| Tuple.second <| step 0 robot1
    -- view <| toString <| Tuple.second <| step Inc (robot2 0)
    view <| toString <| Tuple.second <| step Dec robot3
