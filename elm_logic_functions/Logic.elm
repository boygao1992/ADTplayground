module Logic
    exposing
        ( negate
        , and
        , or
        , when
        , whenOption
        , unless
        , unlessOption
        , ifElse
        )


type alias Pred a =
    a -> Bool


negate : Pred a -> Pred a
negate pred x =
    not <| pred x


and : Pred a -> Pred a -> Pred a
and pred1 pred2 x =
    case ( pred1 x, pred2 x ) of
        ( True, True ) ->
            True

        _ ->
            False


or : Pred a -> Pred a -> Pred a
or pred1 pred2 x =
    case ( pred1 x, pred2 x ) of
        ( False, False ) ->
            False

        _ ->
            True


when : Pred a -> (a -> a) -> a -> a
when pred f x =
    if pred x then
        f x
    else
        x


whenOption : Pred a -> (a -> b) -> a -> Result a b
whenOption pred f x =
    if pred x then
        Result.Ok <| f x
    else
        Result.Err x


unless : Pred a -> (a -> a) -> a -> a
unless pred f x =
    if pred x then
        x
    else
        f x


unlessOption : Pred a -> (a -> b) -> a -> Result b a
unlessOption pred f x =
    if pred x then
        Result.Ok x
    else
        Result.Err <| f x


ifElse : Pred a -> (a -> b) -> (a -> b) -> a -> b
ifElse pred f g x =
    if pred x then
        f x
    else
        g x
