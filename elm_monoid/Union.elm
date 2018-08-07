module Union
    exposing
        ( Union
        , fromSet
        , fromList
        , toSet
        , toList
        , mempty
        , mappend
        , mconcat
        )

import Set exposing (Set)


type Union comparable
    = Union (Set comparable)


fromSet : Set comparable -> Union comparable
fromSet =
    Union


fromList : List comparable -> Union comparable
fromList =
    fromSet << Set.fromList


toSet : Union comparable -> Set comparable
toSet (Union m) =
    m


toList : Union comparable -> List comparable
toList =
    Set.toList << toSet


mempty : Union comparable
mempty =
    Union (Set.empty)


mappend : Union comparable -> Union comparable -> Union comparable
mappend (Union s1) (Union s2) =
    Union <| Set.union s1 s2


mconcat : List (Union comparable) -> Union comparable
mconcat =
    List.foldr mappend mempty


equal : Union comparable -> Union comparable -> Bool
equal (Union s1) (Union s2) =
    (==) 0 <| Set.size <| Set.diff s1 s2
