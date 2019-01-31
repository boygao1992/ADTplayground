

# Status

- [x] GraphQLScalarType
  - [x] GraphQLInt
  - [x] GraphQLFloat
  - [x] GraphQLString
  - [x] GraphQLBoolean
  - [x] GraphQLID

- [x] GraphQLInputObjectType
  - [x] Scalar
  - [x] Record
  - [x] Newtype of Record
  - [x] List of Scalar
  - [x] List of Record
  - [x] List of Newtype
  - [x] Maybe Scalar
  - [x] Maybe Record
  - [x] Maybe Newtype of Record
  - TODO Union (not part of the spec)
    - `../purescript-generics-rep_experiment`
      - module Generic.SumReadPayload (class GenericToConstructor)

- [ ] GraphQLObjectType
  - [x] Nullable and Maybe conversion for `args` and `output`
    - NOTE `source` doesn't need Nullable-Maybe conversion
    - NOTE `args` needs recursive Nullable-Maybe conversion (`class NullableAndMaybeRec`)
    - NOTE `output` needs single-layer Nullable-Maybe conversion (`class NullableAndMaybe`)
    - [x] class ToScalarObjectFieldNoArg
    - [x] class ToScalarObjectFieldWithArgs
    - [x] class ToRelationalObjectFieldNoArg
    - [x] class ToRelationalObjectFieldWithArgs
  - [x] Id ans String conversion for `source`, `args` and `output`
    - [x] class FetchScalarFields (`source`, `output`)
    - [x] class ToInputObject (`args`)
  - [x] Multi-argument input to a Record of arguments conversion for resolvers
    - NOTE mkFn3
  - [x] Aff to Promise conversion
  - [x] `target` type can be `Maybe`
    - [x] class ParseList
      - add a case for (Maybe target)
    - [x] class ToDeps
      - Unit -> Nullable (GraphQLType (Maybe target))
    - [x] class ToRelationalObjectFieldNoArg
      - Unit -> Nullable (GraphQLType (Maybe target))
      - instance toRelationalObjectFieldNoArgImpl
        - (Nullable (GraphQLType (Maybe target)))
    - [x] class ToRelationalObjectFieldWithArgs
      - Unit -> Nullable (GraphQLType (Maybe target))
      - instance toRelationalObjectFieldWithArgsImpl
        - (Nullable (GraphQLType (Maybe target)))
    - [x] class ToRelationalObjectFieldHandleDepListDispatch
      - instance toRelationalObjectFieldHandleDepListDispatchBaseCase
        - NOTE split into two cases
        - Bool.False (Maybe target) target (Maybe dep)
          - unsafeCoerce (depFn unit)
        - Bool.False target target target
          - nonNull (unsafeCoerce (depFn unit))
  - [ ] add Context

- [ ] RootGraphQLObjectType
  - [x] recursively collect all reachable entities from root node
    - class CollectEntities
  - [ ] infer dependency types
  - [ ] inject dependencies
    - [ ] circular reference
      - `../purescript_record_ref`
        - Record.ST.Nested (peekLazyRef)

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
