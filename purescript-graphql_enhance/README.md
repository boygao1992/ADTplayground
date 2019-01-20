
purescript-graphql currently misses Union and Interface from GraphQL API

Union
- `resolveType :: forall a b. GraphQLType b => a -> b`
  - example function dispatches types through duck typing which is not necessary in PureScript
  - can establish mapping from an existential type `a` to a GraphQLType through type class
    - `GraphQLType b <= ToGraphQLType a b | a -> b`

Interface
- TODO

# Type-level

cannot directly pattern match empty row (`()`)
- need to convert row to rowList by `Prim.RowList.RowToList`
- then pattern match empty rowList (`Prim.RowList.Nil`)

# Value without type annotation can be ambiguous at type-level

## Scalars are unambiguous

```purescript
1 :: Int
1.0 :: Number
"a" :: String
'a' :: Char
```

## Closed records with unambiguous values are unambiguous
```purescript
{ id: 0
, name: "wenbo"
} :: Record ( id :: Int
            , name :: String
            )
```

## Records in function arguments can be ambiguous

```purescript
(\{ id } -> id :: String
) :: forall r
   . { id :: String 
     | r
     }
  -> String
```

by default, compiler makes a loose/structural assumption about the records in arguments
- record is open with a free variable `r` declared in scope
  - to denote any other fields that may exist in the record
  - will be filled with a concrete type (potentially an empty Row, `()`) when this function is called with a concrete and closed record

```purescript
getId :: forall r. { id :: String | r } -> String
getId = \{ id } -> id :: String

getId { id: "robot000" } -- r = ()
```

