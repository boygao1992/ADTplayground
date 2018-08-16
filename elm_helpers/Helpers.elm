module Helpers exposing (..)


fanout : (a -> b) -> (a -> c) -> a -> ( b, c )
fanout f g x =
    ( f x, g x )


(&&&) : (a -> b) -> (a -> c) -> a -> ( b, c )
(&&&) =
    fanout


fanin : (a -> c) -> (b -> c) -> Result.Result a b -> c
fanin f g m =
    case m of
        Result.Err a ->
            f a

        Result.Ok b ->
            g b


(|||) : (a -> c) -> (b -> c) -> Result.Result a b -> c
(|||) =
    fanin


functionProduct : (a -> b) -> (c -> d) -> ( a, c ) -> ( b, d )
functionProduct f g ( a, c ) =
    ( f a, g c )


(***) : (a -> b) -> (c -> d) -> ( a, c ) -> ( b, d )
(***) =
    functionProduct
