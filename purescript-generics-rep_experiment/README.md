PureScript compiler Generic support
[purescript/src/Language/PureScript/Sugar/TypeClasses/Deriving.hs](https://github.com/purescript/purescript/blob/master/src/Language/PureScript/Sugar/TypeClasses/Deriving.hs)

Algebraic composition can be generalized into two binary operators on types
- Sum
- Product

which can be further generalized as Tensor Product in Monoidal Category.

Terminals
- `NoConstructors :: ConstructorKind`
- `NoArguments :: ArgumentKind`
- `name :: Symbol`

Non-terminals (with Grammar)
- `Argument :: Type -> ArgumentKind`
- `Constructor :: Symbol -> ArgumentKind -> ConstructorKind`
- `Sum :: ConstructorKind -> ConstructorKind -> ConstructorKind`
- `Product :: ArgumentKind -> ArgumentKind -> ArgumentKind`

`newtype` definition basically wraps a `ArgumentKind` (called anonymous/structural Type) by a `Constructor` with a `Symbol`
- to derive an instance for `Record` (Row Kind, or `ArgumentKind` here) need to wrap it by `newtype` to assign it a unique `Symbol`

# Primitive Types
Product
- `data Tuple :: Type -> Type -> Type`
- `data Record :: # Type -> Type`
- (purescript-functors) Functor.Product
- (Prim) Array
- (purescript-lists) List
- (purescript-infinite-lists) List.Infinite
- (purescript-unordered-collections) HashMap, HashSet
- (purescript-ordered-collections) Map, Set

Sum
- `data Either :: Type -> Type -> Type`
- (purescript-variant) `data Variant :: # Type -> Type`
- (purescript-variant) `data VariantF :: # Type -> Type -> Type`
- (purescript-functors) Functor.Coproduct

Compose
- `compose :: forall b c d a. Semigroupoid a => a c d -> a b c -> a b d`
- (purescript-functors) `Compose :: (Type -> Type) -> (Type -> Type) -> Type -> Type`
- (purescript-free) `Coyoneda`, compose functions that map over a Functor

# Type-Level Programming

## Prim.Row (compiler)

## Prim.Symbol (compiler)

## Prim.TypeError (compiler)
- `kind Doc`
- `class Warn (message :: Doc)`
- `class Fail (message :: Doc)`

## Type.Equality (purescript-type-equality)
```purescript
class TypeEquals a b | a -> b, b -> a where
  to :: a -> b
  from :: b -> a
```
- a type class constraint on Type `a` and `b`
- `a -> b`, `b -> a`
  - `a` is isomorphic to `b`

```purescript
instance refl :: TypeEquals a a where
  to a = a
  from a = a
```
- further restrict isomorphism to equivalence
  - `a -> b` => `a -> a`
  - `b -> a` => `a -> a`
  - without this instance, we can write instances for any two Types to specify their isomorphism
  - with this instance defined, `to` and `from` methods are unnecessary because transforming a Type to itself is pointless
- if `a` and `b` are not of the same Type, then compiler will raise "no type class instance" TypeError

## Type.Data.Symbol (purescript-typelevel-prelude)
type-level literal

`SProxy :: Symbol -> Type`
- uniquely map `Symbol` Kind to `Type` Kind

## Type.Data.Boolean (purescript-typelevel-prelude)
type-level boolean

`kind Boolean`
- `data True :: Boolean`
  - a no-constructor Type `True`, without data constructors, under `Boolean` Kind
  - can be encoded as Type of data/value using `BProxy` and then be able to move around by carrying a reference to that data/value
- `data False :: Boolean`

`BProxy :: Boolean -> Type`
- uniquely map `Boolean` Kind to `Type` Kind
- `BProxy` is "holding" an instance of `Boolean` Kind
- its no-argument data constructor is of the same name, `BProxy`

## Type.Data.Ordering (purescript-typelevel-prelude)
`kind Ordering` (from `Prim.Ordering`)
- `data LT :: Ordering`
- `data GT :: Ordering`
- `data EQ :: Ordering`

"Sum Kind" analogy
- `kind Ordering = LT | GT | EQ` where `LT`, `GT`, `EQ` are Type constructors (which take a terminal object from the Kind category and return a Type)

`OProxy :: Ordering -> Type`
- uniquely map `Ordering` Kind to `Type` Kind

## Data.Variant (purescript-variant)
`FProxy :: (Type -> Type) -> Type`
- uniquely map `Type -> Type` Arrow Kind to `Type` Kind

## Prim.RowList (compiler)
`kind RowList`
- `data Cons :: Symbol -> Type -> RowList -> RowList`
- `data Nil :: RowList`

`class RowToList (row :: # Type) (list :: RowList) | row -> list`
- uniquely map `# Type` Row Kind to `RowList` Kind
- compiler generates a RowList from a **closed** Row behind the scene
  - fields are sorted by label
  - duplicates are preserved in the order they appeared in the Row

## Type.Row (purescript-typelevel-prelude)
`RProxy :: # Type -> Type`
- uniquely map `# Type` Row Kind to `Type` Kind

`class ListToRow (list :: RowList) (row :: # Type) | list -> row`
- inverse of `RowToList`

```purescript
instance listToRowNil
  :: ListToRow Nil ()

instance listToCons
  :: ( ListToRow tail tailRow
     , Cons label ty tailRow row )
=> ListToRow (Cons label ty tail) row
```
- `()` empty Row
- `label ty` a field
  - `label :: Symbol`
  - `ty :: Type`
- `row :: # Type`
  - is equal to `label ty` field concatenated with `tailRow :: # Type`

  
- `RowListRemove`
- `RowListSet`
- `RowListNub`
- `RowAppend`
- `RowApply`

## Type.Row.Homogeneous (purescript-typelevel-prelude)
A type class constraint that enforces all fields of a Record (Row Kind) carrying the same Type.

`class Homogeneous (row :: # Type) fieldType | row -> fieldType`

```purescript
instance homogeneous
  :: ( RowToList row fields
     , HomogeneousRowList fields fieldType )
=> Homogeneous row fieldType
```
- uniquely map `row :: # Type` to `fields :: RowList`
  - from an unordered list/bag, `# Type` Row Kind
    - not set because duplicates are allowed in Row Kind
  - to an ordered list, `RowList` Kind
  - so we can enumerate each element (`field`) sequentially

```purescript
class HomogeneousRowList (fields :: RowList) fieldType | fields -> fieldType

instance homogeneousRowListCons
  :: ( HomogeneousRowList tail fieldType
     , TypeEquals fieldType2 fieldType )
=> HomogeneousRowList (Cons symbol fieldType2 tail) fieldType

instance homogeneousRowListNil :: HomogeneousRowList Nil fieldType
```
- recursively/inductively apply functional dependency `fields -> fieldType` to a `RowList`
  - base case: `Nil :: RowList`
  - induction step: `Cons symbol field tail :: RowList`
    - `tail :: RowList` can be `Cons` or `Nil`. If `Cons` then can be further destructured by `homogenousRowListCons`
- `TypeEquals fieldType fieldType2`
  - enforce `fieldType` to be the same as `fieldType2`
  - if not, compiler will raise "no type class instance" TypeError
  
## Data.Variant (purescript-variant)
`data Variant :: # Type -> Type`

## Data.Functor.Variant (purescript-variant)
`data VariantF (f :: # Type) a`

## Heterogeneous.Mapping (purescript-heterogeneous)
```purescript
class Mapping f a b | f a -> b where
  mapping :: f -> a -> b

class MappingWithIndex f i a b | f i a -> b where
  mappingWithIndex :: f -> i -> a -> b
```

```purescript
instance mappingFunction :: Mapping (a -> b) a b where
  mapping k = k
```
- function application

```purescript
newtype ConstMapping f = ConstMapping f

instance constMapping
  :: Mapping f a b
  => MappingWithIndex (ConstMapping f) ix a b
  where
    mappingWithIndex (ConstMapping f) _ = mapping f
```

```purescript
class HMap f a b | f a -> b where
  hmap :: f -> a -> b

class HMapWithIndex f a b | f a -> b where
  hmapWithIndex :: f -> a -> b
```

## Heterogeneous.Folding (purescript-heterogeneous)

## Type.Prelude (purescript-typelevel-prelude)

## Type.Eval (purescript-typelevel-eval)


# Mapping Example

## Example 1
```haskell
data Fruit
  = Apple Int
  | Banana Int String
```
is equivalent to
```haskell
Sum (Inl (Constructor "Apple" (Argument Int)))
    (Inr (Constructor "Banana" (Product (Argument Int)
                                        (Argument String)
                               )
         )
    )
```

## Example 2: Nested Product
```haskell
data State = State
  { seed :: Int
  , value :: Int
  , iteration :: Int
  }
```
is equivalent to
```
Constructor "State" (Product (Constructor "seed" (Argument Int))
                             (Product (Constructor "value" (Argument Int))
                                      (Constructor "iteration" (Argument Int))
                             )
                    )
```

## Example 3: Nested Sum
```haskell
data Status
  = Idle
  | Playing
  | Paused
```
is equivalent to
```haskell
Sum (Inl (Constructor "Idle" NoArguments))
    (Inr (Sum (Inl (Constructor "Playing" NoArguments))
              (Inr (Constructor "Paused" NoArguments))
         )
    )
```

# Reference

### 1.[Generic Deriving of Generic Traversals](https://arxiv.org/pdf/1805.06798.pdf)
