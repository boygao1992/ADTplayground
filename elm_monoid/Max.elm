module Max
    exposing
        ( Max
        , create
        , mempty
        , mappend
        , mconcat
        , equal
        )


type Max comparable
    = Max (Maybe comparable) -- Maybe.Nothing represents Bottom e.g. -Infinity


create : comparable -> Max comparable
create =
    Max << Maybe.Just


mempty : Max comparable
mempty =
    Max Maybe.Nothing


mappend : Max comparable -> Max comparable -> Max comparable
mappend (Max x) (Max y) =
    case ( x, y ) of
        ( Maybe.Just a, Maybe.Just b ) ->
            Max << Maybe.Just <| max a b

        ( Maybe.Just a, Maybe.Nothing ) ->
            Max x

        ( Maybe.Nothing, Maybe.Just b ) ->
            Max y

        ( Maybe.Nothing, Maybe.Nothing ) ->
            Max (Maybe.Nothing)


mconcat : List (Max comparable) -> Max comparable
mconcat =
    List.foldr mappend mempty


equal : Max comparable -> Max comparable -> Bool
equal (Max x) (Max y) =
    case ( x, y ) of
        ( Maybe.Just a, Maybe.Just b ) ->
            a == b

        _ ->
            False
