module Validation exposing (..)

import Regex


type alias ErrMsg =
    String


type Field raw a
    = Field raw (Validity a)


type Validity a
    = NotValidated
    | Valid a
    | Invalid ErrMsg



-- TODO
-- Con: optional condition is not encoded in Field, need to manually pick the right validate function, i.e. `optional`
-- good thing is, error captured by the type system


type alias OptionalField raw a =
    Field raw (Maybe a)


field : raw -> Field raw a
field raw =
    Field raw NotValidated


getRaw : Field raw a -> raw
getRaw (Field raw _) =
    raw


getValidity : Field raw a -> Validity a
getValidity (Field _ a) =
    a


extractError : Field raw a -> Maybe ErrMsg
extractError field =
    case getValidity field of
        Invalid err ->
            Just err

        _ ->
            Nothing


type alias Validator a b =
    a -> Result ErrMsg b


validate : Validator raw a -> Field raw a -> Field raw a
validate validator field =
    let
        raw =
            getRaw field

        validity =
            getValidity field
    in
        case validity of
            NotValidated ->
                Field raw (raw |> validator |> toValidity)

            _ ->
                field


toValidity : Result ErrMsg a -> Validity a
toValidity result =
    case result of
        Result.Ok a ->
            Valid a

        Result.Err err ->
            Invalid err


optional : Validator String a -> Validator String (Maybe a)
optional validator =
    \s ->
        if s == "" then
            Result.Ok Maybe.Nothing
        else
            validator s |> Result.map Just


map : (a -> b) -> Validity a -> Validity b
map f fa =
    case fa of
        Valid a ->
            Valid <| f a

        NotValidated ->
            NotValidated

        Invalid err ->
            Invalid err


apply : Validity a -> Validity (a -> b) -> Validity b
apply fa ff =
    case ( fa, ff ) of
        ( Valid a, Valid f ) ->
            Valid <| f a

        ( Valid _, NotValidated ) ->
            NotValidated

        ( Valid _, Invalid err ) ->
            Invalid err

        ( NotValidated, _ ) ->
            NotValidated

        ( Invalid err, _ ) ->
            Invalid err


(<*>) : Validity (a -> b) -> Validity a -> Validity b
(<*>) =
    flip apply


pure : a -> Validity a
pure a =
    Valid a


(>=>) : Validator a b -> Validator b c -> Validator a c
(>=>) f g =
    Result.andThen g << f


isNotEmpty : Validator String String
isNotEmpty value =
    if value == "" then
        Result.Err "this field is required"
    else
        Result.Ok value


isEmail : Validator String String
isEmail value =
    let
        regex =
            Regex.regex "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
                |> Regex.caseInsensitive
    in
        if Regex.contains regex value then
            Result.Ok value
        else
            Result.Err "Please enter a valid email address"


isInt : Validator String Int
isInt =
    String.toInt


isPositive : Validator Int Int
isPositive i =
    if i > 0 then
        Result.Ok i
    else
        Result.Err "A positive integer is expected."


isNatural : Validator String Int
isNatural =
    isInt >=> isPositive


isTrue : Validator Bool Bool
isTrue b =
    if b then
        Result.Ok True
    else
        Result.Err "True is expected."


isEqualTo : Field raw a -> Validator a a
isEqualTo otherField =
    \a2 ->
        case getValidity otherField of
            Valid a1 ->
                if a1 == a2 then
                    Result.Ok a2
                else
                    Result.Err "Two fields don't match."

            _ ->
                Result.Ok a2



-- decorate Validator by composition
-- TODO: mapError for Validator


customizeErr : String -> Validator raw a -> Validator raw a
customizeErr err validator =
    validator >> Result.mapError (always err)
