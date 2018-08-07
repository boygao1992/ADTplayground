module All
    exposing
        ( All
        , create
        , mempty
        , mappend
        , mconcat
        , equal
        )


type All
    = All Bool


create : Bool -> All
create =
    All


mempty : All
mempty =
    All True


mappend : All -> All -> All
mappend (All x) (All y) =
    All <| x && y


mconcat : List All -> All
mconcat =
    List.foldr mappend mempty


equal : All -> All -> Bool
equal (All x) (All y) =
    x == y
