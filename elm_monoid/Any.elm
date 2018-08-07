module Any
    exposing
        ( Any
        , create
        , mempty
        , mappend
        , mconcat
        , equal
        )


type Any
    = Any Bool


create : Bool -> Any
create =
    Any


mempty : Any
mempty =
    Any False


mappend : Any -> Any -> Any
mappend (Any x) (Any y) =
    Any <| x || y


mconcat : List Any -> Any
mconcat =
    List.foldr mappend mempty


equal : Any -> Any -> Bool
equal (Any x) (Any y) =
    x == y
