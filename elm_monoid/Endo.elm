module Endo
    exposing
        ( Endo
        , create
        , mempty
        , mappend
        , mconcat
        , appEndo
        )


type Endo a
    = Endo (a -> a)


create : (a -> a) -> Endo a
create =
    Endo


mempty : Endo a
mempty =
    create identity


mappend : Endo a -> Endo a -> Endo a
mappend (Endo f) (Endo g) =
    Endo <| g << f


mconcat : List (Endo a) -> Endo a
mconcat =
    List.foldr mappend mempty


appEndo : a -> Endo a -> a
appEndo x (Endo f) =
    f x
