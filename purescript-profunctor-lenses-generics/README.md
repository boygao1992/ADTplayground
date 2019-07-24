# purescript-profunctor-lenses-generics

A data-type generic solution to derive all possible simple lenses for any deeply nested data structure, inspired by `purescript-generics-rep-optics`.

supported types
- Iso (Profunctor p =>)
  - Newtype
- Lens (Strong p =>)
  - Record
  - Product (Generic =>)
- Prism (Choice p =>)
  - Sum (Generic =>)
    - Maybe
- Index (Wander p =>)
  - Array (Index (Array a) Int a =>)
  - Set (Index (Set v) v Unit =>)
  - Map (Index (Map k v) k v =>)

# Extensibility

Any data-type other than the mentioned ones below needs an instance for `Data.Generic.Rep.Lens (class TTypeFamily)` to be recognized by the solver.

`kind TType`
- `data TScalar`
  - will be treated as leaf node and the solver will stop digging deeper
  - `Int`
  - `Number`
  - `String`
  - `Char`
  - `Boolean`
- `data TRecord`
  - `Record` only, should not be exposed
- `data TIndex`
  - data-type with a `Data.Lens.Index (class Index)` instance
  - `Array a`
- `data TAt`
  - data-type with a `Data.Lens.At (class At)` instance 
    , which is also an instance of `Data.Lens.Index (class Index)`
  - `Set v`
  - `Map k v`
- `data TSum`
  - data-type with a Generic instance
  - `Maybe a`

# Naming Strategy

- Record fields
  - remain the same
- Nested Product
  - `_1`, `_2`, etc.
- Nested Sum
  - `_` prefix + Constructor label
- Index (e.g. `Array`)
  - `ix`
- At (e.g. `Map`, `Set`)
  - `ix`
  - `at`

Leaf nodes have an extra `_` suffix

# Examples

```purescript
data A = A
data B = B
data C = C
derive instance genericA :: Generic A _
derive instance genericB :: Generic B _
derive instance genericC :: Generic C _

data TestSum
  = Empty
  | One A
  | Two A B
  | Three A B C -- Optic' p TestSum { _1 :: A, _2 :: B, _3 :: C }
  | TestRecord { x :: { y :: { z :: Maybe A } } }
  | TestMap (Map String { a :: A })
derive instance genericTestSum :: Generic TestSum _

instance ttypeFamilyA :: TTypeFamily A TSum
instance ttypeFamilyB :: TTypeFamily B TSum
instance ttypeFamilyC :: TTypeFamily C TSum
instance ttypeFamilyTestSum :: TTypeFamily TestSum TSum

class Generic a rep <= ShowGeneric a rep | a -> rep where
  showGenericRep :: Proxy a -> Proxy rep
instance showGeneircImpl :: Generic a rep => ShowGeneric a rep where
  showGenericRep _ = Proxy :: Proxy rep

infixr 2 type Sum as <\/>
infixr 2 type Product as </\>

testsum
  :: Proxy
    ( Constructor "Empty" NoArguments
    <\/> Constructor "One" (Argument A)
    <\/> Constructor "Two" (Argument A </\> Argument B)
    <\/> Constructor "Three" (Argument A </\> Argument B </\> Argument C)
    <\/> Constructor "TestRecord" (Argument { x :: { y :: { z :: Maybe A }}})
    <\/> Constructor "TestMap" (Argument (Map String { a :: A}))
    )
testsum = showGenericRep (Proxy :: Proxy TestSum)

testsumLenses :: forall p. Wander p =>
  { _Empty_ :: Optic' p TestSum Unit
  , _One :: { _A_ :: Optic' p TestSum Unit }
  , _One_ :: Optic' p TestSum A
  , _Two ::
      { _1 :: { _A_ :: Optic' p TestSum Unit }
      , _1_ :: Optic' p TestSum A
      , _2 :: { _B_ :: Optic' p TestSum Unit }
      , _2_ :: Optic' p TestSum B
      }
  , _Two_ :: Optic' p TestSum { _1 :: A, _2 :: B }
  , _Three ::
      { _1 :: { _A_ :: Optic' p TestSum Unit }
      , _1_ :: Optic' p TestSum A
      , _2 :: { _B_ :: Optic' p TestSum Unit }
      , _2_ :: Optic' p TestSum B
      , _3 :: { _C_ :: Optic' p TestSum Unit }
      , _3_ :: Optic' p TestSum C
      }
  , _Three_ :: Optic' p TestSum { _1 :: A, _2 :: B, _3 :: C }
  , _TestRecord ::
      { x :: { y :: { z :: { _Just :: { _A_ :: Optic' p TestSum Unit }
                           , _Just_ :: Optic' p TestSum A
                           , _Nothing_ :: Optic' p TestSum Unit
                           }
                  , z_ :: Optic' p TestSum (Maybe A)
                  }
            , y_ :: Optic' p TestSum { z :: Maybe A }
            }
      , x_ :: Optic' p TestSum { y :: { z :: Maybe A } }
      }
  , _TestRecord_ :: Optic' p TestSum { x :: { y :: { z :: Maybe A } } }
  , _TestMap :: String ->
      { ix ::
          { a :: { _A_ :: Optic' p TestSum Unit }
          , a_ :: Optic' p TestSum A
          }
      , ix_ :: Optic' p TestSum { a :: A }
      , at ::
          { _Just :: { a :: { _A_ :: Optic' p TestSum Unit }
                    , a_ :: Optic' p TestSum A
                    }
          , _Just_ :: Optic' p TestSum { a :: A }
          , _Nothing_ :: Optic' p TestSum Unit
          }
      , at_ :: Optic' p TestSum (Maybe { a :: A })
      }
  , _TestMap_ :: Optic' p TestSum (Map String { a :: A })
  }
testsumLenses = genericLens (Proxy :: Proxy TestSum)

_ThreeC :: Traversal' TestSum C
_ThreeC = testsumLenses._Three._3_

_TestMapAt :: String -> Traversal' TestSum (Maybe { a :: A })
_TestMapAt key = testsumLenses._TestMap key # _.at_

_TestMapIxA :: String -> Traversal' TestSum A
_TestMapIxA key = testsumLenses._TestMap key # _.ix.a_

_TestRecordXYZJust :: Traversal' TestSum A
_TestRecordXYZJust = testsumLenses._TestRecord.x.y.z._Just_

```

# Limitations

### 1. Constraint Kind

Without `Constraint Kind` in Purescript, current solution doesn't assign minimal constraints for each branch but a shared constraint for all the branches, usually `Wander p =>`.

### 2.1 Rank-N Type in Record

`Record` fields are eagerly evaluated thus won't automatically lift types to 2-rank, i.e.
```purescript
foo :: forall a b c. (forall p. p a a -> p s s) -> b -> c
```
is different from
```purescript
type Args a b =
  { _1 :: forall p. p a a -> p s s -- NOTE `p` must be provided at the time a Record of `type Args a b` is constructed
  , _2 :: b
  }
bar :: forall a b c. Args a b -> c
```

A lazy step is needed,
```purescript
type Args' a b =
  { _1 :: Unit -> (forall p. p a a -> p s s)
  , _2 :: b
  }
bar :: forall a b c. Args a b -> c
```

### 2.2 Existential Type in Type-level Functions

Purescript parser and solver doesn't support `forall` keyword in constraints:

```purescript
( Row.Cons "foo" (forall p. p a a -> p s s) from to
) => ...
```
is not a valid expression.

Current solution is to carry a globally defined type variable `p` all around so every lens in the final lens record share the same top-level `forall p`.

Limiting the usage of `unsafeCoerce` as much as possible (twice so far) for the first implementation is a rule of thumb for type-level programming given much less feedback from the compiler comparing to term-level programming.

Later could potentially solve this using `unsafeCoerce` tricks for `Type -> Type -> Type` similar to what `purescript-exists` did for `Type`.

Might even extend this approach to solve the first limitation without `Constraint Kind`.
But an immediate drop back is the recovery of proof will be delayed to the call site which is less user friendly.
