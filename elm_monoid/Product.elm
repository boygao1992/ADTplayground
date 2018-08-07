module Product
    exposing
        ( Product
        , create
        , mempty
        , mappend
        , mconcat
        , equal
        )


type Product number
    = Product number


create : number -> Product number
create =
    Product


mempty : Product number
mempty =
    Product 1


mappend : Product number -> Product number -> Product number
mappend (Product x) (Product y) =
    Product <| x * y


mconcat : List (Product number) -> Product number
mconcat =
    List.foldr mappend mempty


equal : Product number -> Product number -> Bool
equal (Product x) (Product y) =
    x == y
