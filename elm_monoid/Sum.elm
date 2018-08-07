module Sum
    exposing
        ( Sum
        , create
        , mempty
        , mappend
        , mconcat
        , equal
        )


type Sum number
    = Sum number


create : number -> Sum number
create =
    Sum


mempty : Sum number
mempty =
    Sum 0


mappend : Sum number -> Sum number -> Sum number
mappend (Sum x) (Sum y) =
    Sum <| x + y


mconcat : List (Sum number) -> Sum number
mconcat =
    List.foldr mappend mempty


equal : Sum number -> Sum number -> Bool
equal (Sum x) (Sum y) =
    x == y
