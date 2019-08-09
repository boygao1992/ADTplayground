composable queries under Lens interface
- view update problem
  - [Combinators for Bi-Directional Tree Transformations](http://www.cis.upenn.edu/~bcpierce/papers/lenses-toplas-final.pdf)
  - [Boomerang | papers](https://www.seas.upenn.edu/~harmony/)
    - [Relational Lenses: A Language for Updateable Views](http://www.cis.upenn.edu/~bcpierce/papers/dblenses-tr.pdf)
    - [Combinators for bidirectional tree transformations: A linguistic approach to the view-update problem](http://www.cis.upenn.edu/~bcpierce/papers/lenses-toplas-final.pdf)
- parallel/vertical composition of non-disjoint lenses is not law abiding
  - type-level functions can enforce lenses to be disjoint during the composition step (distinct paths is isomorphic to distinct nominal types)
    - [fclabels | parallel composition of lenses for record](http://fvisser.nl/post/2013/okt/1/fclabels-2.0.html)
  - for relational database access (which can be roughly interpreted as nested records access using GraphQL encoding of relational algebra), they are disjoint paths in a graph traversal

potential directions
- build on top of a SQL eDSL
  - why eDSL
    - Raw SQL lives in structureless string space and is not composable.
  - related projects
    - [tathougies/beam](https://github.com/tathougies/beam)
    - [valderman/selda](https://github.com/valderman/selda)
    - [morphismtech/squeal](https://github.com/morphismtech/squeal)
    - [maxigit/sql-fragment](https://github.com/maxigit/sql-fragment)
      - [explanation from author](https://www.reddit.com/r/haskell/comments/8qxvir/a_comparison_among_various_database_edsls_selda/e0qrzri/)
    - [uber/queryparser](https://github.com/uber/queryparser)
  - pro: no data migration needed for existing data in a RDBMS
  - con: codebase of non-trivial SQL eDSLs (with type-safe join operators, which rules out Persist-Esqueleto) are hard to comprehend e.g. Beam
- build on top of a graph database with RDBMS backend support
  - pro: 
    - easier to build composable eDSL for its query language
      - the concept of paths in graph is already backed in e.g. Gremlin
  - con: 
    - its data encoding in RDBMS may not be human-manageable 
      - e.g. massive indexing for graph traversal and fast modification
    - need a migration step for existing data in other storage
- build on top of GraphQL eDSL
  - GraphQL db layer
    - [PostGraphile](https://www.graphile.org/postgraphile/)
    - [Prisma](https://www.prisma.io/)
  - graph databases usually have a GraphQL-like query language (subset but with additional filtering operators)
    - [Dgraph](https://dgraph.io/)
    - [Cayley](https://github.com/cayleygraph/cayley)
  - generality v.s. extra layers of indirection

# Selda

[Reddit - Selda: a monadic database EDSL](https://www.reddit.com/r/haskell/comments/66ih1l/selda_a_monadic_database_edsl/)

> Selda is able to get away safely with a monad because `Query` has a phantom type parameter which stops columns leaking into aggregated subqueries.
> This is a clever trick and I couldn't work out how to do this when I designed Opaleye.
> The key is `Inner` whereas I tried an `ST` style approach which doesn't seem to work.
> I'm not mathematically convinced that it's safe but nonetheless I can't see any reason it shouldn't be.

