module Validation exposing (..)


type alias ErrMsg =
    String


type Field a
    = NotValidated a
    | Valid a
    | Invalid ErrMsg a


type alias Validator a b =
    a -> Result ErrMsg b


validate : Validator a b -> Field a -> Result (Field a) (Field b)
validate validator field =
    case field of
        NotValidated a ->
            case validator a of
                Result.Ok b ->
                    Result.Ok <| Valid b

                Result.Err err ->
                    Result.Err <| Invalid err a

        _ ->
            Result.Err field


map2 : (a -> b -> c) -> Field a -> Field b -> Field c
map2 f fa fb =
    case ( fa, fb ) of
        ( Valid a, Valid b ) ->
            Valid <| f a b

        ( Valid a, _ ) ->
            fb

        ( _, _ ) ->
            fa


displayValue : Field a -> a
displayValue field =
    case field of
        NotValidated val ->
            val

        Valid val ->
            val

        Invalid _ val ->
            val


extractError : Field a -> Maybe ErrMsg
extractError field =
    case field of
        Invalid err val ->
            Just err

        _ ->
            Nothing


isNotEmpty : Validator String String
isNotEmpty value =
    if value == "" then
        Result.Err "this field is required"
    else
        Result.Ok value


isEmail : Validator String String
isEmail value =
    if String.contains "@" value then
        Result.Ok value
    else
        Result.Err "Please enter a valid email address"


isInt : Validator String Int
isInt =
    String.toInt
