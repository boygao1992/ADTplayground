# purescript-profunctor-lenses-generics

A data-type generic solution to derive all possible simple lenses for any deeply nested data structure, inspired by `purescript-generics-rep-optics`.

supported types
- Iso (Profunctor p =>)
  - Newtype (Newtype t a =>)
    - Identity (Newtype (Identity a) a =>)
- Lens (Strong p =>)
  - Record
  - Product (Generic =>)
- Prism (Choice p =>)
  - Sum (Generic =>)
    - Maybe
- Index (Wander p =>)
  - Array (Index (Array a) Int a =>)
- At (Wander p =>)
  - Array (At (Array a) Int a =>)
  - Set (At (Set v) v Unit =>)
  - Map (At (Map k v) k v =>)
- Traversable (Wander p => Traversable t =>)
  - Maybe
  - Array
  - Set
  - Map

# Extensibility

Any data-type other than the ones mentioned below needs an instance for `Data.Generic.Rep.Lens (class TTypeFamily)` to be recognized by the solver.

`kind TType`
- `data TScalar`
  - will be treated as leaf node and the solver will stop digging deeper
  - `Unit`
  - `Int`
  - `Number`
  - `String`
  - `Char`
  - `Boolean`
- `data TNewtype`
  - data-type with a Newtype instance
  - will directly expose its wrapped type (if not desired, use `TSum`)
  - `Identity a`
- `data TRecord`
  - `Record` only, should not be exposed
  - TODO Heterogeneous Mapping/Folding
- `data TSum`
  - data-type with a `Generic` instance
  - `Maybe a`
- `data TIndex`
  - data-type with a `Data.Lens.Index (class Index)` instance
  - `Array a`
- `data TAt`
  - data-type with a `Data.Lens.At (class At)` instance 
    , which is also an instance of `Data.Lens.Index (class Index)`
  - `Array a`
  - `Set v`
  - `Map k v`
- `data TTraversed`
  - data-type with a `Traversable` instance
  - `Maybe`
  - `Array`
  - `Set`
  - `Map k`

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
newtype SimpleNewtype = SimpleNewtype Unit
derive instance genericA :: Generic A _
derive instance genericB :: Generic B _
derive instance genericC :: Generic C _
derive instance newtypeSimpleNewtype :: Newtype SimpleNewtype _

data TestSum
  = Empty
  | One A
  | Two A B
  | Three A B C -- Optic' p TestSum { _1 :: A, _2 :: B, _3 :: C }
  | TestRecord { x :: { y :: { z :: Maybe A } } }
  | TestMap (Map String { a :: A })
  | TestNewtype SimpleNewtype
derive instance genericTestSum :: Generic TestSum _

instance ttypeFamilyA :: TTypeFamily A TSum
instance ttypeFamilyB :: TTypeFamily B TSum
instance ttypeFamilyC :: TTypeFamily C TSum
instance ttypeFamilySimpleNewtype :: TTypeFamily SimpleNewtype TNewtype
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
    <\/> Constructor "TestNewtype" (Argument SimpleNewtype)
    )
testsum = showGenericRep (Proxy :: Proxy TestSum)

testsumLenses ::
  { _Empty_ :: CPrism' TestSum Unit
  , _One :: { _A_ :: CPrism' TestSum Unit }
  , _One_ :: CPrism' TestSum A
  , _Two ::
       { _1 :: { _A_ :: CTraversal' TestSum Unit }
       , _1_ :: CTraversal' TestSum A
       , _2 :: { _B_ :: CTraversal' TestSum Unit }
       , _2_ :: CTraversal' TestSum B
       }
  , _Two_ :: CTraversal' TestSum { _1 :: A, _2 :: B }
  , _Three ::
       { _1 :: { _A_ :: CTraversal' TestSum Unit }
       , _1_ :: CTraversal' TestSum A
       , _2 :: { _B_ :: CTraversal' TestSum Unit }
       , _2_ :: CTraversal' TestSum B
       , _3 :: { _C_ :: CTraversal' TestSum Unit }
       , _3_ :: CTraversal' TestSum C
       }
  , _Three_ :: CTraversal' TestSum { _1 :: A, _2 :: B, _3 :: C }
  , _TestRecord ::
       { x :: { y :: { z :: { _Just :: { _A_ :: CTraversal' TestSum Unit }
                         , _Just_ :: CTraversal' TestSum A
                         , _Nothing_ :: CTraversal' TestSum Unit
                         , traversed :: { _A_ :: CTraversal' TestSum Unit }
                         , traversed_ :: CTraversal' TestSum A
                         }
                   , z_ :: CTraversal' TestSum (Maybe A)
                   }
             , y_ :: CTraversal' TestSum { z :: Maybe A }
             }
       , x_ :: CTraversal' TestSum { y :: { z :: Maybe A } }
       }
  , _TestRecord_ :: CPrism' TestSum { x :: { y :: { z :: Maybe A } } }
  , _TestMap ::
      { at :: String ->
          { _Just ::
              { a :: { _A_ :: CTraversal' TestSum Unit }
              , a_ :: CTraversal' TestSum A
              }
          , _Just_ :: CTraversal' TestSum { a :: A }
          , _Nothing_ :: CTraversal' TestSum Unit
          , traversed ::
              { a :: { _A_ :: CTraversal' TestSum Unit }
              , a_ :: CTraversal' TestSum A
              }
              , traversed_ :: CTraversal' TestSum { a :: A }
          }
      , at_ :: String -> CTraversal' TestSum (Maybe { a :: A })
      , ix :: String ->
          { a :: { _A_ :: CTraversal' TestSum Unit }
          , a_ :: CTraversal' TestSum A
          }
      , ix_ :: String -> CTraversal' TestSum { a :: A }
      , traversed ::
          { a :: { _A_ :: CTraversal' TestSum Unit }
          , a_ :: CTraversal' TestSum A
          }
      , traversed_ :: CTraversal' TestSum { a :: A }
      }
  , _TestMap_ :: CPrism' TestSum (Map String { a :: A })
  , _TestNewtype :: CPrism' TestSum Unit
  , _TestNewtype_ :: CPrism' TestSum SimpleNewtype
  }
testsumLenses = genericLens (Proxy :: Proxy TestSum)

_TestSumThreeC :: CTraversal' TestSum C
_TestSumThreeC = testsumLenses._Three._3_

_TestSumTestMapAt :: String -> CTraversal' TestSum (Maybe { a :: A })
_TestSumTestMapAt key = testsumLenses._TestMap.at_ key

_TestSumTestMapIxA :: String -> CTraversal' TestSum A
_TestSumTestMapIxA key = testsumLenses._TestMap.ix key # _.a_

_TestSumTestRecordXYZJust :: CTraversal' TestSum A
_TestSumTestRecordXYZJust = testsumLenses._TestRecord.x.y.z._Just_

```

# Limitations

### CLenses

On call site, user needs to unwrap CLenses `newtype` constructors (i.e. `COptic'`, `CIso'`, `CLens'`, `CPrism'`, `CTraversal'`) by calling corresponding `unwarpC` functions in `Data.Generic.Rep.Lens.Constraints.Simple` the same way as recovering ALenses by corresponding `clone` functions but with less performance penalty.

User can use the generic functions defined in `Data.Generic.Rep.Lens.Constraint.Simple` like `cPreview'` and `cSet'` instead of the original ones to automatically unwrap these constructors but only a handful of operators have currently been implemented.

### ~~1. Constraint Kind~~

Without `Constraint Kind` in Purescript, current solution doesn't assign minimal constraints for each branch but a shared constraint for all the branches, usually `Wander p =>`.

### ~~2.1 Rank-N Type in Record~~

`Record` fields are eagerly evaluated thus won't automatically lift types to 2-rank, i.e.
```purescript
foo :: forall s a b c. (forall p. p a a -> p s s) -> b -> c
```
is different from
```purescript
type Args s a b =
  { _1 :: forall p. p a a -> p s s -- NOTE `p` must be provided at the time a Record of `type Args a b` is constructed
  , _2 :: b
  }
bar :: forall s a b c. Args s a b -> c
```

A lazy step is needed,
```purescript
type Args' s a b =
  { _1 :: Unit -> (forall p. p a a -> p s s)
  , _2 :: b
  }
bar :: forall s a b c. Args a b -> c
```

### ~~2.2 Existential Type in Type-level Functions~~

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

```purescript
-- Order-3 Existential Type

foreign import data Exists3 :: ((Type -> Type -> Type) -> Type) -> Type

mkExists3 :: forall f p. f p -> Exists3 f
mkExists3 = unsafeCoerce

runExists3 :: forall f r. (forall p. f p -> r) -> Exists3 f -> r
runExists3 = unsafeCoerce

-- Order-3 Existential Types with Constraints

foreign import data CProfunctor :: ((Type -> Type -> Type) -> Type) -> Type
foreign import data CStrong :: ((Type -> Type -> Type) -> Type) -> Type
foreign import data CChoice :: ((Type -> Type -> Type) -> Type) -> Type
foreign import data CWander :: ((Type -> Type -> Type) -> Type) -> Type

mkCWander :: forall f p. Wander p => f p -> CWander f
mkCWander = unsafeCoerce

runCWander :: forall f r. (forall p. Wander p => f p -> r) -> CWander f -> r
runCWander = unsafeCoerce

-- Lenses

newtype OpticF s a p = OpticF (p a a -> p s s)
derive instance newtypeOpticF :: Newtype (OpticF s a p) _

newtype COptic' s a = COptic' (Exists3 (OpticF s a))
-- ~ forall p. p a a -> p s s
newtype CIso' s a = CIso' (CProfunctor (OpticF s a))
-- ~ forall p. Profunctor p => p a a -> p s s
newtype CLens' s a = CLens' (CStrong (OpticF s a))
-- ~ forall p. Strong p => p a a -> p s s
newtype CPrism' s a = CPrism' (CChoice (OpticF s a))
-- ~ forall p. Choice p => p a a -> p s s
newtype CTraversal' s a = CTraversal' (CWander (OpticF s a))
-- ~ forall p. Wander p => p a a -> p s s

runCTraversal' :: forall s a r. (forall p. Wander p => (p a a -> p s s) -> r) -> CTraversal' s a -> r
runCTraversal' f (CTraversal' cwander) = runCWander (f <<< unwrap) cwander

-- Usage

testsumLenses ::
  { _Empty_ :: CPrism' TestSum Unit
  , _One :: { _A_ :: CTraversal' TestSum Unit }
  , _One_ :: CPrism' TestSum A
  }
testsumLenses = genericCLens (Proxy :: Proxy TestSum)

testsumSample :: TestSum
testsumSample = One A

testOneA :: Maybe Unit
testOneA = runCTraversal (testsumLenses._One._A_) (testsumSample ^? _)
```

Found a built-in solution with `AIso`, `ALens`, `APrism`, `ATraversal`:
```purescript
lensesInRecord :: { name :: forall a. ALens' { name :: a } a }
lensesInRecord =
  { name: lens _.name $ _ { name = _ }
  }

_name :: forall a. Lens' { name :: a } a
_name = cloneLens lensesInRecord.name
```
