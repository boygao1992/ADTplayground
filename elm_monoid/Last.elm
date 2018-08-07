module Last
    exposing
        ( Last
        , create
        , map
        , mempty
        , mappend
        , mconcat
        , equal
        )


type Last a
    = Last (Maybe a)


create : a -> Last a
create =
    Last << Maybe.Just


map : (a -> b) -> Last a -> Last b
map f (Last x) =
    Last << Maybe.map f <| x


mempty : Last a
mempty =
    Last Maybe.Nothing


mappend : Last a -> Last a -> Last a
mappend fa fb =
    let
        (Last y) =
            fb
    in
        case y of
            Maybe.Just _ ->
                fb

            Maybe.Nothing ->
                fa


mconcat : List (Last a) -> Last a
mconcat =
    List.foldr mappend mempty


equal : Last a -> Last a -> Bool
equal (Last a) (Last b) =
    -- open world assumption: Maybe.Nothing /= Maybe.Nothing
    case ( a, b ) of
        ( Maybe.Just x, Maybe.Just y ) ->
            x == y

        _ ->
            False
