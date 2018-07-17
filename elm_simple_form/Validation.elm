module Validation exposing (..)


type alias ErrMsg =
    String



-- type Field a
--     = NotValidated a
--     | Valid a
--     | Invalid ErrMsg a


type Field a
    = NotValidated String
    | Valid a
    | Invalid ErrMsg String


type alias Validator a b =
    a -> Result ErrMsg b



-- validate : Validator a b -> Field a -> Result (Field a) (Field b)
-- validate validator field =
--     case field of
--         NotValidated a ->
--             case validator a of
--                 Result.Ok b ->
--                     Result.Ok <| Valid b
--                 Result.Err err ->
--                     Result.Err <| Invalid err a
--         _ ->
--             Result.Err field


validate : Validator String a -> Field a -> Field a
validate validator field =
    case field of
        NotValidated value ->
            case validator value of
                Result.Ok a ->
                    Valid a

                Result.Err err ->
                    Invalid err value

        _ ->
            field


map : (a -> b) -> Field a -> Field b
map f fa =
    case fa of
        Valid a ->
            Valid <| f a

        NotValidated s ->
            NotValidated s

        Invalid err s ->
            Invalid err s


apply : Field a -> Field (a -> b) -> Field b
apply fa ff =
    case ( fa, ff ) of
        ( Valid a, Valid f ) ->
            Valid <| f a

        ( Valid _, NotValidated s ) ->
            NotValidated s

        ( Valid _, Invalid err s ) ->
            Invalid err s

        ( NotValidated s, _ ) ->
            NotValidated s

        ( Invalid err s, _ ) ->
            Invalid err s


(<*>) : Field (a -> b) -> Field a -> Field b
(<*>) =
    flip (apply)


pure : a -> Field a
pure a =
    Valid a



-- map2 : (a -> b -> c) -> Field a -> Field b -> Field c
-- map2 f fa fb =
--     case ( fa, fb ) of
--         ( Valid a, Valid b ) ->
--             Valid <| f a b
--         ( Valid a, _ ) ->
--             fb
--         ( _, _ ) ->
--             fa


map2 : (a -> b -> c) -> Field a -> Field b -> Field c
map2 f fa fb =
    case ( fa, fb ) of
        ( Valid a, Valid b ) ->
            Valid <| f a b

        ( Valid _, NotValidated s ) ->
            NotValidated s

        ( Valid _, Invalid err s ) ->
            Invalid err s

        ( NotValidated s, _ ) ->
            NotValidated s

        ( Invalid err s, _ ) ->
            Invalid err s


(>=>) : Validator a b -> Validator b c -> Validator a c
(>=>) f g =
    -- (>=>) / chain : Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
    -- Result.andThen : (a -> Result x b) -> Result x a -> Result x b
    -- (>>=) : Monad m => (a -> m b) -> m a -> m b
    Result.andThen g << f



-- displayValue : Field a -> a
-- displayValue field =
--     case field of
--         NotValidated val ->
--             val
--         Valid val ->
--             val
--         Invalid _ val ->
--             val


displayValue : (a -> String) -> Field a -> String
displayValue render field =
    case field of
        NotValidated s ->
            s

        Valid a ->
            render a

        Invalid err s ->
            s


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


isPositive : Validator Int Int
isPositive i =
    if i >= 0 then
        Result.Ok i
    else
        Result.Err "A positive integer is expected."


isNatural : Validator String Int
isNatural =
    isInt >=> isPositive
