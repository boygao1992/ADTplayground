module Min
    exposing
        ( Min
        , create
        , mempty
        , mappend
        , mconcat
        , equal
        )


type Min comparable
    = Min (Maybe comparable) -- Maybe.Nothing represents Top e.g. +Infinity


create : comparable -> Min comparable
create =
    Min << Maybe.Just


mempty : Min comparable
mempty =
    Min Maybe.Nothing


mappend : Min comparable -> Min comparable -> Min comparable
mappend (Min x) (Min y) =
    case ( x, y ) of
        ( Maybe.Just a, Maybe.Just b ) ->
            Min << Maybe.Just <| min a b

        ( Maybe.Just a, Maybe.Nothing ) ->
            Min x

        ( Maybe.Nothing, Maybe.Just b ) ->
            Min y

        ( Maybe.Nothing, Maybe.Nothing ) ->
            Min (Maybe.Nothing)


mconcat : List (Min comparable) -> Min comparable
mconcat =
    List.foldr mappend mempty


equal : Min comparable -> Min comparable -> Bool
equal (Min x) (Min y) =
    case ( x, y ) of
        ( Maybe.Just a, Maybe.Just b ) ->
            a == b

        _ ->
            False
