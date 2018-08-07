module First
    exposing
        ( First
        , create
        , map
        , mempty
        , mappend
        , mconcat
        , equal
        )


type First a
    = First (Maybe a)


create : a -> First a
create =
    First << Maybe.Just


map : (a -> b) -> First a -> First b
map f (First x) =
    First << Maybe.map f <| x


mempty : First a
mempty =
    First Maybe.Nothing


mappend : First a -> First a -> First a
mappend fa fb =
    let
        (First x) =
            fa
    in
        case x of
            Maybe.Just _ ->
                fa

            Maybe.Nothing ->
                fb


mconcat : List (First a) -> First a
mconcat =
    List.foldr mappend mempty


equal : First a -> First a -> Bool
equal (First a) (First b) =
    -- open world assumption: Maybe.Nothing /= Maybe.Nothing
    case ( a, b ) of
        ( Maybe.Just x, Maybe.Just y ) ->
            x == y

        _ ->
            False
