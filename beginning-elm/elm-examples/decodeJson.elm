module Main exposing (..)

import Date exposing (Date)
import Dict exposing (Dict)
import Html exposing (text, ul, li)
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
        , map
        , map2
        , map4
        , map5
        , map6
          -- decodeString :: Decoder a -> String(Json) -> Result String a
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
          -- parse `null` and return the given value if Result.Ok
          -- null : a -> Decoder a
        , null
          -- ignore the Json string and return the given value in Result.Ok
          -- succeed : a -> Decoder a
        , succeed
          -- ignore the Json string and return the given error string in Result.Err
          -- fail : String -> Decoder a
        , fail
          -- andThen : (a -> Decoder b) -> Decoder a -> Decoder b
        , andThen
          -- handle nullable field
          -- nullable : Decoder a -> Decoder (Maybe a)
        , nullable
          -- handle optional field
          -- maybe : Decoder a -> Decoder (Maybe a)
        , maybe
          -- handle a list of possible structures encoded in Decoder
          -- oneOf : List (Decoder a) -> Decoder a
        , oneOf
          -- list : Decoder a -> Decoder (List a)
        , list
          -- drill down nested field
          -- at : List String -> Decoder a -> Decoder a
        , at
        , lazy
        )
import Json.Decode.Extra
    exposing
        ( -- a Natural Transformation from Result to Decoder
          -- fromResult :: Result String a -> Decoder a
          fromResult
        , parseInt
          -- date : Decoder Date
        , date
          -- Apply f => f (a -> b) -> f a -> f b, left-associative
          -- (|:) : Decoder (a -> b) -> Decoder a -> Decoder b
        , (|:)
        )
import Json.Decode.Pipeline
    exposing
        ( decode
        , optional
        , optionalAt
        , required
        , requiredAt
        )


{- 1. course overview

   When developing real world applications in Elm you will encounter the problem of decoding JSON data from an API.

   Elms static types need data converted from JSON to something Elm can understand. Elm exposes decoders for us to do just that.

   In this course, you will learn how to decode individual values, simple JSON objects, lists, and nested objects. You will also learn how to handle nullable and optional fields in Elm as well as returning data or an error object when requests are bad.

   After taking this course, you will have a solid grasp on converting JSON into Elm so you can pull real world data into your application

-}
{- 2. Understand Elm Decoders and the `decodeString` Function


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

-}
{- 3. Understand Elm Decoder and `decodeString` Function -}
{- 4. Decode JSON primitives into Elm

      -- Result is a tagged union type that captures potentially parsing errors
      type Result error value
          = Ok value
          | Err error
      -- a instance of Monad
      -- andThen = flip(Bind), able to chain computation with effects
      andThen : (a -> Result x b) -> Result x a -> Result x b
      andThen callback result =
          case result of
            Ok value ->
              callback value

            Err msg ->
              Err msg

   > "1" |> decodeString int
   Ok 1 : Result.Result String Int

   > "1" |> decodeString string
   Err "Execting a String but instead got : 1" : Result.Result String String

   > """ "1" """ |> decodeString string
   Ok "1" : Result.Result String String

   > "true" |> decodeString bool
   Ok True : Result.Result String Bool

   -- Difference between JS and Elm type system: Float type
   > "1" |> decodeString float
   Ok 1 : Result.Result String Float

   > "1.1" |> decodeString float
   Ok 1.1 : Result.Result String Float

   > "1.1" |> decodeString int
   Err "Expecting an Int but instead got: 1.1" : Result.Result String Int

-}
{- 5. Use `map` to transform data while decoding JSON into Elm
   -- Decoder is a Functor
   -- map :: (a -> value) -> Json.Decode.Decoder a -> Json.Decode.Decoder value


   data : String
   data =
       """
   {
       "id": 123,
       "email": "joe@domain.net",
       "isPremium": true
   }
   """



   -- type = data in Haskell, LHS is the type constructor, RHS is the data constructor


   type Membership
       = Standard
       | Premium


   type alias User =
       -- a data constructor of 3 variables :: Int -> String -> Bool -> User
       { id : Int
       , email : String
       , membership : Membership
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
           (field "isPremium" membership)


   membership : Decoder Membership
   membership =
       let
           toMembership : Bool -> Membership
           toMembership b =
               case b of
                   True ->
                       Premium

                   False ->
                       Standard
       in
           -- Decoder Bool -> Decoder Membership
           bool |> map toMembership


   main : Html.Html msg
   main =
       data
           |> decodeString userDecoder
           |> toString
           |> text

-}
{- 6. Use `andThen` to transform data while decoding JSON into Elm


   data : String
   data =
       """
   {
       "id": 123,
       "email": "joe@domain.net",
       "isPremium": true,
       "gender": "male"
   }
   """


   type Membership
       = Standard
       | Premium


   type Gender
       = Male
       | Female


   type alias User =
       -- a data constructor of 4 variables :: Int -> String -> Membership -> Gender -> User
       { id : Int
       , email : String
       , membership : Membership
       , gender : Gender
       }


   membership : Decoder Membership
   membership =
       let
           -- Boolean is exhaustible/finite
           toMembership : Bool -> Membership
           toMembership b =
               case b of
                   True ->
                       Premium

                   False ->
                       Standard
       in
           -- Decoder Bool -> Decoder Membership
           bool |> map toMembership


   gender : Decoder Gender
   gender =
       let
           -- Could be implemented on top of Result
           -- But in Elm, succeed/fail are implemented as a Union type in JS
           -- ./elm-stuff/packages/elm-lang/core/5.1.1/src/Native/Json.js
           -- for performance?
           -- in Section 7, `fromResult` is used to bridge Result and Decoder
           -- toGender :: String -> Result String Gender
           toGender : String -> Decoder Gender
           toGender s =
               case s of
                   "male" ->
                       -- Result.Ok Male
                       succeed Male

                   "female" ->
                       -- Result.Ok Female
                       succeed Female

                   _ ->
                       -- Result.Err "invalid"
                       fail (s ++ " is not a valid gender")
       in
           string |> andThen toGender


   userDecoder : Decoder User
   userDecoder =
       map4 User
           -- Decoder Int
           (field "id" int)
           -- Decoder String
           (field "email" string)
           -- Decoder Membership
           (field "isPremium" membership)
           -- Decoder Gender
           (field "gender" gender)


   main : Html.Html msg
   main =
       data
           |> decodeString userDecoder
           |> toString
           |> text

-}
{- 7. Decode a JSON string and convert it to an Elm `Int`
   -- elm-community/json-extra
   {-
      type Result error value
          = Ok value
          | Err error

      -- a simple implementation based on pattern matching on Result
      fromResult : Result String a -> Decoder a
      fromResult result =
          case result of
              Ok successValue ->
                  succeed successValue

              Err errorMessage ->
                  fail errorMessage
   -}


   data : String
   data =
       """
   {
       "id": "123",
       "email": "joe@domain.net",
       "isPremium": true,
       "gender": "male"
   }
   """


   type Membership
       = Standard
       | Premium


   type Gender
       = Male
       | Female



   {- Json schema of original data
      {
        id: String, -- from legacy api
        email: String,
        isPremium: Bool,
        gender: String
      }
   -}


   type alias User =
       { id : Int
       , email : String
       , membership : Membership
       , gender : Gender
       }


   membership : Decoder Membership
   membership =
       let
           toMembership : Bool -> Membership
           toMembership b =
               case b of
                   True ->
                       Premium

                   False ->
                       Standard
       in
           bool |> map toMembership


   gender : Decoder Gender
   gender =
       let
           -- rebuilt using Result and `fromResult`
           toGender : String -> Result String Gender
           toGender s =
               case s of
                   "male" ->
                       Result.Ok Male

                   "female" ->
                       Result.Ok Female

                   _ ->
                       Result.Err (s ++ " is not a valid gender")
       in
           string |> andThen (toGender >> fromResult)


   userDecoder : Decoder User
   userDecoder =
       map4 User
           -- Decoder String -> Decoder Int
           -- (field "id" string |> andThen (String.toInt >> fromResult))
           (field "id" parseInt)
           -- Decoder String
           (field "email" string)
           -- Decoder Membership
           (field "isPremium" membership)
           -- Decoder Gender
           (field "gender" gender)


   main : Html.Html msg
   main =
       data
           |> decodeString userDecoder
           |> toString
           |> text

-}
{- 8. Decode JSON Objects into Elm Records Using `field` and `mapX` -}
{- 9. Handle nullable and optional Fields when Decoding Json into Elm


   data : String
   data =
       """
   {
       "id": "123",
       "email": "joe@domain.net",
       "isPremium": true,
       "gender": null
   }
   """


   type Membership
       = Standard
       | Premium


   type Gender
       = Male
       | Female



   {- Json schema of original data
      {
        id: String, -- from legacy api
        email: String,
        isPremium: Bool,
        gender: null
      }
   -}


   type alias User =
       { id : Int
       , email : String
       , membership : Membership

       -- both nullable and optional field have the same type signature: Maybe a
       , gender : Maybe Gender
       }


   membership : Decoder Membership
   membership =
       let
           toMembership : Bool -> Membership
           toMembership b =
               case b of
                   True ->
                       Premium

                   False ->
                       Standard
       in
           bool |> map toMembership


   gender : Decoder Gender
   gender =
       let
           -- rebuilt using Result and `fromResult`
           toGender : String -> Result String Gender
           toGender s =
               case s of
                   "male" ->
                       Result.Ok Male

                   "female" ->
                       Result.Ok Female

                   _ ->
                       Result.Err (s ++ " is not a valid gender")
       in
           string |> andThen (toGender >> fromResult)


   userDecoder : Decoder User
   userDecoder =
       map4 User
           -- Decoder String -> Decoder Int
           -- (field "id" string |> andThen (String.toInt >> fromResult))
           (field "id" parseInt)
           -- Decoder String
           (field "email" string)
           -- Decoder Membership
           (field "isPremium" membership)
           -- Decoder Gender -> Decoder Maybe(Gender)
           -- for optional field
           -- (maybe <| field "gender" gender)
           -- for nullable field
           (field "gender" <| nullable gender)


   main : Html.Html msg
   main =
       data
           |> decodeString userDecoder
           |> toString
           |> text

-}
{- 10. Decode Json into Elm Lists and Arrays


   data : String
   data =
       """
   {
       "id": "123",
       "email": "joe@domain.net",
       "isPremium": true,
       "gender": null,
       "notifications": [
         { "title" : "Welcome back!", "message": "we've been missing you"},
         { "title" : "Weather alert", "message": "expect stormy weather"}
       ]
   }
   """


   type Membership
       = Standard
       | Premium


   type Gender
       = Male
       | Female


   type alias Notification =
       { title : String
       , message : String
       }



   {- Json schema of original data
      {
         properties: {
           id: String,
           email: String,
           isPremium: Bool,
           gender: String,
           notifications: {
             type: array,
             items: { type: string }
           },
         }
         required: [id, email, isPremium, notifications]
         // optional: [gender]
      }
   -}


   type alias User =
       { id : Int
       , email : String
       , membership : Membership
       , gender : Maybe Gender
       , notifications : List Notification
       }


   membership : Decoder Membership
   membership =
       let
           toMembership : Bool -> Membership
           toMembership b =
               case b of
                   True ->
                       Premium

                   False ->
                       Standard
       in
           bool |> map toMembership


   gender : Decoder Gender
   gender =
       let
           toGender : String -> Result String Gender
           toGender s =
               case s of
                   "male" ->
                       Result.Ok Male

                   "female" ->
                       Result.Ok Female

                   _ ->
                       Result.Err (s ++ " is not a valid gender")
       in
           string |> andThen (toGender >> fromResult)


   notification : Decoder Notification
   notification =
       map2 Notification
           (field "title" string)
           (field "message" string)


   userDecoder : Decoder User
   userDecoder =
       map5 User
           -- Decoder String -> Decoder Int
           -- (field "id" string |> andThen (String.toInt >> fromResult))
           (field "id" parseInt)
           -- Decoder String
           (field "email" string)
           -- Decoder Membership
           (field "isPremium" membership)
           -- Decoder Maybe(Gender)
           (field "gender" <| nullable gender)
           -- Decoder List Notification
           (field "notifications" <| list notification)


   main : Html.Html msg
   main =
       data
           |> decodeString userDecoder
           |> toString
           |> text


-}
{- 11. Decode JSON dates into Elm


   data : String
   data =
       """
   {
       "id": "123",
       "email": "joe@domain.net",
       "isPremium": true,
       "gender": null,
       "dateOfBirth": "Sun Jan 07 2018 00:18:17 GMT+0100 (CET)",
       "notifications": [
         { "title" : "Welcome back!", "message": "we've been missing you"},
         { "title" : "Weather alert", "message": "expect stormy weather"}
       ]
   }
   """


   type Membership
       = Standard
       | Premium


   type Gender
       = Male
       | Female


   type alias Notification =
       { title : String
       , message : String
       }



   {- Json schema of original data
      {
         properties: {
           id: String,
           email: String,
           isPremium: Bool,
           gender: String,
           notifications: {
             type: array,
             items: { type: string }
           },
         }
         required: [id, email, isPremium, notifications]
         // optional: [gender]
      }
   -}


   type alias User =
       { id : Int
       , email : String
       , membership : Membership
       , gender : Maybe Gender
       , dateOfBirth : Date
       , notifications : List Notification
       }


   membership : Decoder Membership
   membership =
       let
           toMembership : Bool -> Membership
           toMembership b =
               case b of
                   True ->
                       Premium

                   False ->
                       Standard
       in
           bool |> map toMembership


   gender : Decoder Gender
   gender =
       let
           toGender : String -> Result String Gender
           toGender s =
               case s of
                   "male" ->
                       Result.Ok Male

                   "female" ->
                       Result.Ok Female

                   _ ->
                       Result.Err (s ++ " is not a valid gender")
       in
           string |> andThen (toGender >> fromResult)


   notification : Decoder Notification
   notification =
       map2 Notification
           (field "title" string)
           (field "message" string)


   userDecoder : Decoder User
   userDecoder =
       map6 User
           -- Decoder String -> Decoder Int
           -- (field "id" string |> andThen (String.toInt >> fromResult))
           (field "id" parseInt)
           -- Decoder String
           (field "email" string)
           -- Decoder Membership
           (field "isPremium" membership)
           -- Decoder Maybe(Gender)
           (field "gender" <| nullable gender)
           (field "dateOfBirth" date)
           -- Decoder List Notification
           (field "notifications" <| list notification)


   main : Html.Html msg
   main =
       data
           |> decodeString userDecoder
           |> toString
           |> text


-}
{- 12. Decode nested JSON Objects into flat Elm Records


   data : String
   data =
       """
   {
       "id": "123",
       "email": "joe@domain.net",
       "isPremium": true,
       "profile": {
         "gender": null,
         "dateOfBirth": "Sun Jan 07 2018 00:18:17 GMT+0100 (CET)"
       },
       "notifications": [
         { "title" : "Welcome back!", "message": "we've been missing you"},
         { "title" : "Weather alert", "message": "expect stormy weather"}
       ]
   }
   """


   type Membership
       = Standard
       | Premium


   type Gender
       = Male
       | Female


   type alias Notification =
       { title : String
       , message : String
       }



   {- Json schema of original data
      {
         properties: {
           id: String,
           email: String,
           isPremium: Bool,
           profile: {
             type: object,
             properties: {
               gender: String,
               dateOfBirth: String
             },
             required: [dateOfBirth]
             // optional: [gender]
           }
           notifications: {
             type: array,
             items: { type: string }
           },
         }
         required: [id, email, isPremium, profile, notifications]
      }
   -}


   type alias User =
       { id : Int
       , email : String
       , membership : Membership
       , gender : Maybe Gender
       , dateOfBirth : Date
       , notifications : List Notification
       }


   membership : Decoder Membership
   membership =
       let
           toMembership : Bool -> Membership
           toMembership b =
               case b of
                   True ->
                       Premium

                   False ->
                       Standard
       in
           bool |> map toMembership


   gender : Decoder Gender
   gender =
       let
           toGender : String -> Result String Gender
           toGender s =
               case s of
                   "male" ->
                       Result.Ok Male

                   "female" ->
                       Result.Ok Female

                   _ ->
                       Result.Err (s ++ " is not a valid gender")
       in
           string |> andThen (toGender >> fromResult)


   notification : Decoder Notification
   notification =
       map2 Notification
           (field "title" string)
           (field "message" string)


   userDecoder : Decoder User
   userDecoder =
       map6 User
           -- Decoder String -> Decoder Int
           -- (field "id" string |> andThen (String.toInt >> fromResult))
           (field "id" parseInt)
           -- Decoder String
           (field "email" string)
           -- Decoder Membership
           (field "isPremium" membership)
           -- Decoder Maybe(Gender)
           (at [ "profile", "gender" ] <| nullable gender)
           -- Decoder Date
           (at [ "profile", "dateOfBirth" ] date)
           -- Decoder List Notification
           (field "notifications" <| list notification)


   main : Html.Html msg
   main =
       data
           |> decodeString userDecoder
           |> toString
           |> text


-}
{- 13. Decode large JSON objects into Elm with `json-extra`


   data : String
   data =
       """
   {
       "id": "123",
       "email": "joe@domain.net",
       "isPremium": true,
       "profile": {
         "gender": null,
         "dateOfBirth": "Sun Jan 07 2018 00:18:17 GMT+0100 (CET)"
       },
       "notifications": [
         { "title" : "Welcome back!", "message": "we've been missing you"},
         { "title" : "Weather alert", "message": "expect stormy weather"}
       ]
   }
   """


   type Membership
       = Standard
       | Premium


   type Gender
       = Male
       | Female


   type alias Notification =
       { title : String
       , message : String
       }



   {- Json schema of original data
      {
         properties: {
           id: String,
           email: String,
           isPremium: Bool,
           profile: {
             type: object,
             properties: {
               gender: String,
               dateOfBirth: String
             },
             required: [dateOfBirth]
             // optional: [gender]
           }
           notifications: {
             type: array,
             items: { type: string }
           },
         }
         required: [id, email, isPremium, profile, notifications]
      }
   -}


   type alias User =
       { id : Int
       , email : String
       , membership : Membership
       , gender : Maybe Gender
       , dateOfBirth : Date
       , notifications : List Notification
       }


   membership : Decoder Membership
   membership =
       let
           toMembership : Bool -> Membership
           toMembership b =
               case b of
                   True ->
                       Premium

                   False ->
                       Standard
       in
           bool |> map toMembership


   gender : Decoder Gender
   gender =
       let
           toGender : String -> Result String Gender
           toGender s =
               case s of
                   "male" ->
                       Result.Ok Male

                   "female" ->
                       Result.Ok Female

                   _ ->
                       Result.Err (s ++ " is not a valid gender")
       in
           string |> andThen (toGender >> fromResult)


   notification : Decoder Notification
   notification =
       map2 Notification
           (field "title" string)
           (field "message" string)


   userDecoder : Decoder User
   userDecoder =
       -- Decoder is an Applicative
       -- success = pure, lift a function into Applicative
       succeed User
           -- (|:) = (<*>), apply
           |: (field "id" parseInt)
           |: (field "email" string)
           |: (field "isPremium" membership)
           |: (at [ "profile", "gender" ] <| nullable gender)
           |: (at [ "profile", "dateOfBirth" ] date)
           |: (field "notifications" <| list notification)


   main : Html.Html msg
   main =
       data
           |> decodeString userDecoder
           |> toString
           |> text


-}
{- 14. Decode large JSON objects into Elm with `elm-decode-pipeline` -}


data : String
data =
    """
{
    "id": "123",
    "email": "joe@domain.net",
    "isPremium": true,
    "profile": {
      "gender": null,
      "dateOfBirth": "Sun Jan 07 2018 00:18:17 GMT+0100 (CET)"
    },
    "notifications": [
      { "title" : "Welcome back!", "message": "we've been missing you"},
      { "title" : "Weather alert", "message": "expect stormy weather"}
    ]
}
"""


type Membership
    = Standard
    | Premium


type Gender
    = Male
    | Female


type alias Notification =
    { title : String
    , message : String
    }



{- Json schema of original data
   {
      properties: {
        id: String,
        email: String,
        isPremium: Bool,
        profile: {
          type: object,
          properties: {
            gender: String,
            dateOfBirth: String
          },
          required: [dateOfBirth]
          // optional: [gender]
        }
        notifications: {
          type: array,
          items: { type: string }
        },
      }
      required: [id, email, isPremium, profile, notifications]
   }
-}


type alias User =
    { id : Int
    , email : String
    , membership : Membership
    , gender : Maybe Gender
    , dateOfBirth : Date
    , notifications : List Notification
    }


membership : Decoder Membership
membership =
    let
        toMembership : Bool -> Membership
        toMembership b =
            case b of
                True ->
                    Premium

                False ->
                    Standard
    in
        bool |> map toMembership


gender : Decoder Gender
gender =
    let
        toGender : String -> Result String Gender
        toGender s =
            case s of
                "male" ->
                    Result.Ok Male

                "female" ->
                    Result.Ok Female

                _ ->
                    Result.Err (s ++ " is not a valid gender")
    in
        string |> andThen (toGender >> fromResult)


notification : Decoder Notification
notification =
    map2 Notification
        (field "title" string)
        (field "message" string)


userDecoder : Decoder User
userDecoder =
    decode User
        |> required "id" parseInt
        |> required "email" string
        |> required "isPremium" membership
        |> optionalAt [ "profile", "gender" ] (gender |> map Just) Nothing
        |> requiredAt [ "profile", "dateOfBirth" ] date
        |> optional "notifications" (list notification) []



{- the upper is equivalent to -}


userDecoder2 : Decoder User
userDecoder2 =
    -- Decoder User
    optional "notifications"
        (list notification)
        []
        (-- Decoder (f -> User)
         requiredAt [ "profile", "dateOfBirth" ]
            date
            (-- Decoder (e -> (f -> User))
             optionalAt [ "profile", "gender" ]
                (gender |> map Just)
                Nothing
                (-- Decoder (d -> (e -> f -> User))
                 required "isPremium"
                    membership
                    (-- Decoder (c -> (d -> e -> f -> User))
                     required "email"
                        string
                        (-- Decoder (b -> (c -> d -> e -> f -> User))
                         required "id"
                            parseInt
                            (-- Decoder (a -> (b -> c -> d -> e -> f -> User))
                             decode User
                            )
                        )
                    )
                )
            )
        )



{- Explanation

   (|>) :: a -> (a -> b) -> b
   x |> f = f x

   in Ramda, it's called
   applyTo :: a -> (a -> b) -> b
   _curry2(
    function applyTo(x, f) {
      return f(x)
    }
   )

   basically, inverse the control of a function call.
   Or in a different perspective, a unary function and a value are dual operators.
   You can apply a function to a value, and you can apply a value to a function.
   e.g. Row vector, Column vector

   in Json.Decode.Extra,
   andMap : Decoder a -> Decoder (a -> b) -> Decoder b
   andMap =
       map2 (|>)

   its infix version,
   (|:) : Decoder (a -> b) -> Decoder a -> Decoder b
   (|:) =
       flip andMap

   in Json.Decode.Pipeline, the same function is called `custom`
   custom : Decoder a -> Decoder (a -> b) -> Decoder b
   custom =
       Decode.map2 (|>)

   map2 : (a -> b -> value) -> Decoder a -> Decoder b -> Decoder value
   map2 = lift2 in Apply
   lift2 :: Apply m => (a -> b -> c) -> m a -> m b -> m c

   in map2 (|>),
   map2 :: (a -> (a -> b) -> b) -> Decoder a -> Decoder (a -> b) -> Decoder b

   instead of a unary function (a -> b),
   a n-ary function (x1 -> x2 -> ... -> y)
   can be treated as (x1 -> (x2 -> ... -> y)), a higher order function
   where x1 = a, (x2 -> ... -> y) = b

-}
{- Understanding of Decoder

   Decoder is a Reader and Result. (latter is obvious, by definition)
   (Reader implements Functor, Apply, Applicative, Monad)
   The environment which all these chained Decoders take in is the JSON string to be parsed.
   These Decoders are chained sequentially (as Applicative into a "bigger" Decoder) but function independently. (a typical scenario to have parallel computing and use an Applicative to coordinate the end result)

   It can be modeled this way because parsing of the JSON string is not part of the computation of each Decoder.
   It's parsed only once using native JSON.parse() into native JS Object which is a dictionary in its C++ runtime.
   (./elm-stuff/packages/elm-lang/core/5.1.1/src/Json/Decode.elm
    decodeString : Decoder a -> String -> Result String a
    decodeString =
      Native.Json.runOnString
   )
   (./elm-stuff/packages/elm-lang/core/5.1.1/src/Native/Json.js
    function runOnString
   )
   And then each decoder is just a lense/query on this dictionary to grab the required field, which is efficient (O(1)).

   decodeString :: Decoder a -> String -> Result String a
   decodeString = flip(runReader)
   runReader    :: e -> Reader e a  -> a
   in this case, e is String
   Since Decoder encapsulates Result, so the type signature slightly different from generic Reader

   the Result is to handle potential errors during the parsing.
   One of the Decoders fails, then the entire parsing fails with an Err message.
-}
-- main : Html.Html msg
-- main =
--     data
--         |> decodeString userDecoder
--         |> toString
--         |> text
{- 15. Automatically generating JSON Decoders in Elm

   [Json to Elm](http://eeue56.github.io/json-to-elm/)

   generate code for original JSON schema as a starting point

   then customize data transformations and specify required fields
-}
{- 16. Decode JSON Error/Success data into Elm union types -}


successResponse : String
successResponse =
    """
{
  "data": {
    "id": "123",
    "email": "joe@domain.net",
    "isPremium": true,
    "profile": {
      "gender": null,
      "dateOfBirth": "Sun Jan 07 2018 00:18:17 GMT+0100 (CET)"
    },
    "notifications": [
      { "title" : "Welcome back!", "message": "we've been missing you"},
      { "title" : "Weather alert", "message": "expect stormy weather"}
    ]
  }
}
"""


failureResponse : String
failureResponse =
    """
{
  "error": {
    "message": "wrong password"
  }
}
"""


responseDecoder : Decoder (Result String User)
responseDecoder =
    oneOf
        [ field "data" user |> map Result.Ok
        , at [ "error", "message" ] string |> map Result.Err
        ]


user : Decoder User
user =
    decode User
        |> required "id" parseInt
        |> required "email" string
        |> required "isPremium" membership
        |> optionalAt [ "profile", "gender" ] (gender |> map Just) Nothing
        |> requiredAt [ "profile", "dateOfBirth" ] date
        |> optional "notifications" (list notification) []



-- main : Html.Html msg
-- main =
--     failureResponse
--         |> decodeString responseDecoder
--         |> toString
--         |> text
{- 17. Decode a JSON Object into an Elm Dictionary -}


jsonDict : String
jsonDict =
    """
{
  "about_us": "Sobre nosotros",
  "location": "Ubicacion",
  "terms_of_service": "Politicas"
}
"""



-- main : Html.Html msg
-- main =
--     let
--         navigationItems =
--             [ "about_us", "terms_of_service" ]
--         dict =
--             jsonDict
--                 -- only works for key-value pairs whose values are of the same type
--                 -- dict :: Decoder a -> Decoder (Dict String a)
--                 |> decodeString (Json.Decode.dict string)
--                 |> Result.withDefault Dict.empty
--         translate key =
--             Dict.get key dict |> Maybe.withDefault key
--     in
--         ul []
--             (navigationItems
--                 |> List.map translate
--                 |> List.map (\item -> li [] [ text item ])
--             )
{- 18. Decode a JSON Category Tree or Navigation Tree into Elm -}


categoryTreeJson : String
categoryTreeJson =
    """
{
  "value" : "root",
  "children" : [{
    "value" : "Bikes",
    "children" : [
      { "value" : "Cruiser bikes" },
      { "value" : "Road bikes" },
      { "value" : "MTB bikes" }
    ]
  }, {
    "value" : "Components",
    "children" : [
      { "value": "Saddles" },
      {
        "value": "Brakes",
        "children": [
          { "value": "Brake levers" },
          { "value": "Brake cables" },
          { "value": "Brake pads" }
        ]
      },
      { "value": "Cassettes" },
      { "value": "Chains" },
      { "value": "Pedals" },
      { "value": "Stems" }
    ]
  }, {
    "value" : "Wheels & tyres",
    "children" : [
      { "value" : "Hubs" },
      { "value" : "Spokes" },
      { "value" : "Rims" },
      {
        "value" : "Tyres",
        "children" : [
          { "value" : "Cruiser tyres" },
          { "value" : "Road tyres" },
          { "value" : "MTB tyres" }
        ]
      }
    ]
  }]
}
"""


type Node
    = Node String (List Node)



-- node : Decoder Node
-- node =
--     map2 Node
--         (field "value" string)
--         (field "children" (list <| lazy <| \_ -> node)
--             |> maybe
--             |> map (Maybe.withDefault [])
--         )
-- render : List Node -> Html.Html msg
-- render nodes =
--     let
--         renderNode (Node value children) =
--             li [] [ text value, render children ]
--     in
--         case nodes of
--             [] ->
--                 text ""
--             _ ->
--                 ul [] (nodes |> List.map renderNode)
-- main : Html.Html msg
-- main =
--     categoryTreeJson
--         |> decodeString node
--         |> \result ->
--             case result of
--                 Result.Ok (Node _ children) ->
--                     render children
--                 Result.Err err ->
--                     text err
{- my implementation -}


type Tree a
    = Leaf a
    | Branch a (List (Tree a))


type alias CategoryTree =
    Tree String


fromNode : Node -> CategoryTree
fromNode (Node value children) =
    case children of
        [] ->
            Leaf value

        _ ->
            Branch value <| List.map fromNode children


node : Decoder Node
node =
    decode Node
        |> required "value" string
        |> optional "children" (list <| lazy <| \_ -> node) []


render : CategoryTree -> Html.Html msg
render ct =
    case ct of
        Leaf value ->
            li [] [ text value ]

        Branch value children ->
            li []
                [ text value
                , ul [] (List.map render children)
                ]


main : Html.Html msg
main =
    categoryTreeJson
        |> decodeString node
        |> Result.map fromNode
        |> \result ->
            case result of
                Result.Ok tree ->
                    render tree

                Result.Err err ->
                    text err
