composable queries under Lens interface
- view update problem
  - [Combinators for Bi-Directional Tree Transformations](http://www.cis.upenn.edu/~bcpierce/papers/lenses-toplas-final.pdf)
  - [Boomerang | papers](https://www.seas.upenn.edu/~harmony/)
    - [Relational Lenses: A Language for Updateable Views](http://www.cis.upenn.edu/~bcpierce/papers/dblenses-tr.pdf)
    - [Combinators for bidirectional tree transformations: A linguistic approach to the view-update problem](http://www.cis.upenn.edu/~bcpierce/papers/lenses-toplas-final.pdf)
- parallel/vertical composition of non-disjoint lenses is not law abiding
  - type-level functions can enforce lenses to be disjoint during the composition step (distinct paths are isomorphic to distinct nominal types)
    - [fclabels | parallel composition of lenses for record](http://fvisser.nl/post/2013/okt/1/fclabels-2.0.html)
  - for relational database access (which can be roughly interpreted as nested records access using GraphQL encoding of relational algebra), they are disjoint paths in a graph traversal
- Monadic Lenses
  - [Reflections on Monadic Lenses](https://arxiv.org/abs/1601.02484)
  - [lens-action](http://hackage.haskell.org/package/lens-action)
  - [extensible-monadic-lenses](https://www.reddit.com/r/haskell/comments/8gc8p0/extensible_monadic_lenses/)
    - [response from Edward Kmett on lens laws](https://www.reddit.com/r/haskell/comments/8gc8p0/extensible_monadic_lenses/dybzem6/)
      - lens-like but not "well behaved"
    - [Lenses for philosophers](https://julesh.com/2018/08/16/lenses-for-philosophers/)
      > The lens laws are really the crux of everything I’m saying. From my earlier general and informal definition of lenses, it is possible to justify the (associative) composition law of lenses, but it is not possible to justify the lens laws. The laws are firmly properties of update actions — similar laws also appear in Hoare logic for example. For many other lens-like things the laws are either false, or more likely fail to even type-check. I considered calling the general thing, namely “things that look like lenses and compose like lenses”, something like ‘pre-lenses’ or ‘quasi-lenses’. But I have the opinion that if it walks like a duck and composes like a duck, then you ought to call it a duck.
      >
      > The lens laws are undoubtedly important, and not just because they’re true in the case of updates. Very well behaved lenses are equivalent to the much simpler constant-complement lenses, and are also equivalently coalgebras of a comonad. Most of the theoretical work that uses the term ‘lenses’ takes the laws as given. Many bx researchers define ‘lenses’ to be what FGMPS called ‘well behaved lenses’, and consider that something not satisfying the lens laws is no lens at all.
- eDSL
  - Monadic Bind
    - [Simple and Compositional Reification of Monadic Embedded Languages](https://svenssonjoel.github.io/writing/bb.pdf)
      - [svenssonjoel/Robot](https://github.com/svenssonjoel/Robot)
        - [Mops](https://github.com/svenssonjoel/Robot/blob/master/Comp.hs)
          - re-discovery of `Free`
          - auto injection of sub-language through type-level pattern matching
    - [Church encoding | Asymptotic Improvement of Computations over Free Monads](https://web.archive.org/web/20110607105555/http://www.iai.uni-bonn.de/~jv/mpc08.pdf)
      - [Control.Monad.Trans.Free.Church](https://hackage.haskell.org/package/free-5.1.1/docs/Control-Monad-Trans-Free-Church.html)
      - [Free 0.1.6](https://github.com/purescript/purescript-free/blob/v0.1.6/src/Control/Monad/Free.purs)
    - [Reflection without Remorse](http://okmij.org/ftp/Haskell/zseq.pdf)
      - [Free 0.6.1](https://github.com/purescript/purescript-free/blob/v0.6.1/src/Control/Monad/Free.purs)
  - Data Parallel Computation
    - [svenssonjoel/EmbArBB](https://github.com/svenssonjoel/EmbArBB)
      - [Parallel Programming in Haskell Almost for Free](http://svenssonjoel.github.io/writing/almost_free.pdf)
    - [svenssonjoel/Obsidian](https://github.com/svenssonjoel/Obsidian)
      - [Design Exploration through Code-generating DSLs](http://dl.acm.org/citation.cfm?id=2626374)
      - [A Language for Hierarchical Data Parallel Design-space Exploration on GPUs](http://journals.cambridge.org/action/displayFulltext?type=1&fid=10242178&jid=JFP&volumeId=26&issueId=-1&aid=10242156)
      - [(Thesis) Embedded Languages for Data-Parallel Programming](http://svenssonjoel.github.io/thesis/thesis.pdf)
    - [accelerate](https://www.acceleratehs.org/publications.html)
      - OPLSS 2018 - Purely Functional Array Programming - Gabrielle Keller
        - [1/3](https://www.youtube.com/watch?v=RCPsNceeXk4)
        - [2/3](https://www.youtube.com/watch?v=on1ReFZi31w)
        - [3/3](https://www.youtube.com/watch?v=G3aMnU7n0v4)
  - Parametric Higher Order Abstract Syntax (PHOAS)
    - [PHOAS For Free | Edward Kmett](https://www.reddit.com/r/haskell/comments/1mo59h/phoas_for_free_by_edward_kmett/)
      > Just like with `bound` we can build an `indexed` version of this construction that permits us to write **strongly typed EDSLs**. That is how PHOAS is usually presented in Coq after all.
    - [Write you a Haskell > Higher Order Interpreters > PHOAS](http://dev.stephendiehl.com/fun/evaluation.html#parametric-higher-order-abstract-syntax-phoas)
- characterize (Free) Profunctor by data structure
  - [Profunctor Optics: Modular Data Accessors](https://arxiv.org/abs/1703.10857)
  - [ElvishJerricco/fraxl](https://github.com/ElvishJerricco/fraxl)
    - [Applicative Sorting](https://elvishjerricco.github.io/2017/03/23/applicative-sorting.html)
    - [Profunctors, Arrows, & Static Analysis](https://elvishjerricco.github.io/2017/03/10/profunctors-arrows-and-static-analysis.html)
      > Free PreArrow
      > ```haskell
      > data Free p a b where
      >   Hom :: (a -> b) -> Free p a b
      >   Comp :: forall x. p x b -> Free p a x -> Free p a b -- NOTE (<<<)
      > ```
      - [Notions_of_Computation_as_Monoids]()
        > Free Weak Arrows
        > ```haskell
        > class Profunctor p => WeakArrow p where
        >   arr :: forall a b. (a -> b) -> p a b
        >   (>>>) :: forall a x b. p a x -> p x b -> p a b -- NOTE
        > data Free p a b
        >   Hom :: (a -> b) -> Free p a b
        >   Comp :: forall x. p a x -> Free p x b -> Free p a b
        > ```
        > Profunctor Tensor
        > ```haskell
        > data (p ⊗ q) a b where
        >   PCom :: forall a b. p a x -> q x b -> (p ⊗ q) a b
        > instance (Profunctor p, Profunctor q) => Profunctor (p ⊗ q) where
        >   dimap m1 m2 (PCom p q) = PCom (dimap m1 id p) (dimap id m2 q)
        > ```
        - [Profunctor Optics: Modular Data Accessors](https://arxiv.org/abs/1703.10857)
          > Profunctor generalizes Kleisli arrow `a -> f b`
          > [Data.Profunctor.Star](https://pursuit.purescript.org/packages/purescript-profunctor/4.1.0/docs/Data.Profunctor.Star)
          - [The Essence of the Iterator Pattern](https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf)
            > 3.3 Combining applicative functors
            > ```haskell
            > -- NOTE Prod :: m a -> n a -> (m ⊠ n) a
            > data (m ⊠ n) a = Prod { pfst :: m a, psnd :: n a }
            > -- NOTE a -> m b       ~ p a b
            > --      a -> n b       ~ q a b
            > --      a → (m ⊠ n) b ~ (p ⊗ q) a b
            > -- (Profunctor p, Profunctor q) => p a b -> q a b -> (p ⊗ q) a b
            > (⊗) :: (Functor m, Functor n) ⇒ (a → m b) → (a → n b) → (a → (m ⊠ n) b)
            > (f ⊗ g) x = Prod (f x) (g x)
            > instance (Applicative m, Applicative n) ⇒ Applicative (m ⊠ n) where
            >   pure x = Prod (pure x) (pure x)
            >   mf <*> mx = Prod (pfst mf <*> pfst mx) (psnd mf <*> psnd mx)
            >
            > data (m ⊡ n) a = Comp { unComp :: m (n a) }
            > -- NOTE unified under Profunctor Tensor the same way as above
            > (⊙) :: (Functor n, Functor m) ⇒ (b → n c) → (a → m b) → (a → (m ⊡ n) c)
            > f ⊙ g = Comp ◦ fmap f ◦ g
            > instance (Applicative m, Applicative n) ⇒ Applicative (m ⊡ n) where
            >   pure x = Comp (pure (pure x))
            >   (Comp mf ) <*> (Comp mx) = Comp (pure (<*>) <*> mf <*> mx)
            > ```
            > The two operators ⊗ and ⊙ allow us to combine idiomatic computations in two different ways; we call them **parallel and sequential composition**, respectively.

    - [Kleisli Functors](https://elvishjerricco.github.io/2016/10/12/kleisli-functors.html)
    - [Abstracting Async.Concurrently](https://elvishjerricco.github.io/2016/09/17/abstracting-async-concurrently.html)
    - [More on Applicative Effects in Free Monads](https://elvishjerricco.github.io/2016/04/13/more-on-applicative-effects-in-free-monads.html)
    - [Applicative Effects in Free Monads](https://elvishjerricco.github.io/2016/04/08/applicative-effects-in-free-monads.html)

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

[Scoping Monadic Relational Database Queries](https://ekblad.cc/pubs/selda-paper.pdf)

[Reddit - Selda: a monadic database EDSL](https://www.reddit.com/r/haskell/comments/66ih1l/selda_a_monadic_database_edsl/)

> Selda is able to get away safely with a monad because `Query` has a phantom type parameter which stops columns leaking into aggregated subqueries.
> This is a clever trick and I couldn't work out how to do this when I designed Opaleye.
> The key is `Inner` whereas I tried an `ST` style approach which doesn't seem to work.
> I'm not mathematically convinced that it's safe but nonetheless I can't see any reason it shouldn't be.

- [ ] Selda
  - [ ] Backend
  - [x] Query
    - [x] Type
  - [x] SQL
    - [x] Print
      - [x] Config
  - [ ] Table
    - [x] Type
    - [x] Validation
    - [x] Compile
  - [ ] Column
    - TODO
  - [ ] Compile
  - [ ] Debug
  - [x] Exp
  - [ ] FieldSelectors
  - [ ] Frontend
  - [x] Generic
    - DONE use Record instead of HList
  - [x] Inner
  - [ ] MakeSelectors
  - [ ] Migration
  - [x] Nullable
  - [ ] Prepared
  - [x] Selectors
  - [ ] SqlRow
    - TODO use Record instead of HList
  - [ ] SqlType
    - TODO DateTime, Day, Time
  - [x] Transform
  - [x] Types
  - [x] Unsafe
  - [ ] Validation

