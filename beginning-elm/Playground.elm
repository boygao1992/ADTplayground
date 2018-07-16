module Playground exposing (..)

import Html
import String


escapeEarth : Float -> Float -> String
escapeEarth velocity speed =
    let
        escapeVelocityInKmPerSec =
            11.186

        orbitalSpeedInKmPerSec =
            7.67
    in
        if velocity > escapeVelocityInKmPerSec then
            "Godspeed"
        else if speed == orbitalSpeedInKmPerSec then
            "Stay in orbit"
        else
            "Come back"


speed : Float -> Float -> Float
speed distance time =
    distance / time


time : number -> number -> number
time startTime endTime =
    endTime - startTime



{- pipe -}
{-
   main : Html.Html msg
   main =
       time 2 3
           |> speed 7.67
           |> escapeEarth 11
           |> Html.text
-}
{- compose
   main =
       Html.text
           <| escapeEarth 11
           <| speed 7.67
           <| time 2 3
-}


weekday : Int -> String
weekday dayInNumber =
    case dayInNumber of
        0 ->
            "Sunday"

        1 ->
            "Monday"

        2 ->
            "Tuesday"

        3 ->
            "Wednsday"

        4 ->
            "Thursday"

        5 ->
            "Friday"

        6 ->
            "Saturday"

        _ ->
            "Unknown day"


escapeSingleQuote =
    '\''


backslash =
    '\\'


escapeDoubleQuote =
    " left \" quote \" right "


stringLength =
    String.length "7 words"


stringInfixConcat =
    "Hello" ++ " " ++ "World"


stringAppend =
    "3"
        |> String.append "2"
        |> String.append "1"


stringFromChar =
    String.fromChar 'a'


main : Html.Html msg
main =
    Html.text <|
        stringAppend
