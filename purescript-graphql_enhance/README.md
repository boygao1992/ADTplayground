
purescript-graphql currently misses Union and Interface from GraphQL API

Union
- `resolveType :: forall a b. GraphQLType b => a -> b`
  - example function dispatches types through duck typing which is not necessary in PureScript
  - can establish mapping from an existential type `a` to a GraphQLType through type class
    - `GraphQLType b <= ToGraphQLType a b | a -> b`

Interface
- TODO
