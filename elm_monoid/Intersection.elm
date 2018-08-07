module Intersection
    exposing
        ( Intersection
        , fromSet
        , fromList
        , toSet
        , toList
        , mempty
        , mappend
        , mconcat
        )

import Set exposing (Set)


type Intersection comparable
    = Intersection (Maybe (Set comparable)) -- Maybe.Nothing denotes a set of everything


fromSet : Set comparable -> Intersection comparable
fromSet =
    Intersection << Maybe.Just


fromList : List comparable -> Intersection comparable
fromList =
    fromSet << Set.fromList


toSet : Intersection comparable -> Set comparable
toSet (Intersection m) =
    Maybe.withDefault Set.empty <| m


toList : Intersection comparable -> List comparable
toList =
    Set.toList << toSet


mempty : Intersection comparable
mempty =
    Intersection (Maybe.Nothing)


mappend : Intersection comparable -> Intersection comparable -> Intersection comparable
mappend (Intersection m1) (Intersection m2) =
    case ( m1, m2 ) of
        ( Maybe.Just s1, Maybe.Just s2 ) ->
            Intersection << Maybe.Just <| Set.intersect s1 s2

        ( Maybe.Just _, Maybe.Nothing ) ->
            Intersection m1

        ( Maybe.Nothing, Maybe.Just _ ) ->
            Intersection m2

        ( Maybe.Nothing, Maybe.Nothing ) ->
            Intersection Maybe.Nothing


mconcat : List (Intersection comparable) -> Intersection comparable
mconcat =
    List.foldr mappend mempty


equal : Intersection comparable -> Intersection comparable -> Bool
equal (Intersection m1) (Intersection m2) =
    case ( m1, m2 ) of
        ( Maybe.Just s1, Maybe.Just s2 ) ->
            (==) 0 <| Set.size <| Set.diff s1 s2

        _ ->
            False
