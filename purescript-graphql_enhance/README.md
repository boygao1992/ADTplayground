

# Status

- [x] GraphQLScalarType
  - [x] GraphQLInt
  - [x] GraphQLFloat
  - [x] GraphQLString
  - [x] GraphQLBoolean
  - [x] GraphQLID

- [ ] GraphQLInputObjectType
  - [x] Scalar
  - [x] Record
  - [x] Newtype of Record
  - [x] List of Scalar
  - [x] List of Record
  - [x] List of Newtype
  - [x] Maybe Scalar
  - [ ] Maybe Record
  - [ ] Maybe Newtype of Record
  - [ ] Union (non-nested)

- [ ] GraphQLObjectType
  - [x] Nullable and Maybe conversion for `args` and `output`
    - NOTE `source` doesn't need Nullable-Maybe conversion
    - NOTE `args` needs recursive Nullable-Maybe conversion (`class NullableAndMaybeRec`)
    - NOTE `output` needs single-layer Nullable-Maybe conversion (`class NullableAndMaybe`)
    - [x] ToScalarObjectFieldNoArg
    - [x] ToScalarObjectFieldWithArgs
    - [x] ToRelationalObjectFieldNoArg
    - [x] ToRelationalObjectFieldWithArgs
  - [x] Id ans String conversion for `source`, `args` and `output`
    - [x] FetchScalarFields (`source`, `output`)
    - [x] ToInputObject (`args`)
  - [x] Multi-argument input to a Record of arguments conversion for resolvers
    - NOTE mkFn3
  - [x] Aff to Promise conversion
  - [ ] add Context

- [ ] description

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

# Union and Interface

Union
- `resolveType :: forall a b. GraphQLType b => a -> b`
  - example function dispatches types through duck typing which is not necessary in PureScript

Interface
- TODO
