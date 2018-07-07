module Main exposing (..)

import Html exposing (..)
import Json.Decode
    exposing
        ( -- JS primitives: string, bool, int, float
          -- Elm types / data structures: Maybe, List, Array, Dict, List of (key-value) pairs
          Decoder
        )
import Json.Decode
    exposing
        ( -- map3 :: (a -> b -> c -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder value
          map3
          -- decodeString :: Decoder a -> String -> Result String a
        , decodeString
          -- field :: String -> Decoder a -> Decoder a
          -- field lifts a primitive type into a named type
        , field
          -- int :: Decoder Int (data constructor)
        , int
          -- string :: Decoder String
        , string
          -- bool :: Decoder Bool
        , bool
        )


{- 1. course overview

   When developing real world applications in Elm you will encounter the problem of decoding JSON data from an API.

   Elms static types need data converted from JSON to something Elm can understand. Elm exposes decoders for us to do just that.

   In this course, you will learn how to decode individual values, simple JSON objects, lists, and nested objects. You will also learn how to handle nullable and optional fields in Elm as well as returning data or an error object when requests are bad.

   After taking this course, you will have a solid grasp on converting JSON into Elm so you can pull real world data into your application

-}
{- 2. Understand Elm Decoders and the `decodeString` Function -}


data : String
data =
    """
{
    "id": 123,
    "email": "joe@domain.net",
    "isPremium": true
}
"""


type alias User =
    -- a data constructor of 3 variables :: Int -> String -> Bool -> User
    { id : Int
    , email : String
    , isPremium : Bool
    }


userDecoder : Decoder User
userDecoder =
    -- lift3 :: Applicative m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
    -- map3 :: (Int -> String -> Bool -> User) -> Decoder Int -> Decoder String -> Decoder Bool -> Decoder User
    map3 User
        -- Decoder Int
        (field "id" int)
        -- Decoder String
        (field "email" string)
        -- Decoder Bool
        (field "isPremium" bool)


main : Html msg
main =
    data
        |> decodeString userDecoder
        |> toString
        |> text
