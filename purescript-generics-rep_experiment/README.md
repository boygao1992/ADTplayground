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

## Type.Proxy (purescript-proxy)
A proxy value carrying a single phantom Type.
Can be used for remote type-level coordination which cannot simply be scoped.

`data Proxy a = Proxy`
- `Proxy :: Type -> Type`

### Example (from purescript-proxy)
```purescript
class AjaxResponse a where
  responseType :: Proxy a -> ResponseType
  fromResponse :: Foreign -> a

type URL = String

-- | a general function from Ajax library to send an Ajax request
makeRequest :: URL -> ResponseType -> Aff Foreign
makeRequest = ...

-- | a less general function which encapsulates the decoding of `Foreign` response into Type `a`
fetchData :: forall a. (AjaxResponse a) => URL -> Aff a
fetchData url = fromResponse <$> makeRequest url (responseType (Proxy :: Proxy a))
```
- no value of Type `a` is available or necessary at the time the request is sent
- use `Proxy` to inject the expected Type, `a`, of response

```purescript
main :: Effect Unit
main = do
  movie <- liftAff ((fetchData "http://purescript.org/movie") :: Aff String) -- coerce `a` into `String`, then `fetchData` will use `AjaxResponse String` instance
  log movie
```
- seems to me there's no need to use `Proxy` in this case
- `responseType` and `fromResponse` are "entangled" under `AjaxResponse` Type Class which nicely injects `a` through scope

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

## Data.HObject (purescript-homogeneous-objects)
  
## Data.Variant (purescript-variant)
`data Variant :: # Type -> Type`

## Data.Functor.Variant (purescript-variant)
`data VariantF (f :: # Type) a :: # Type -> Type -> Type`

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

## Record (purescript-record)
```purescript
class EqualFields (rs :: RowList) (row :: # Type) | rs -> row where
  equalFields :: RLProxy rs -> Record row -> Record row -> Boolean
```
- two 

```purescript
instance equalFieldsCons
  ::
  ( IsSymbol name
  , Eq ty
  , Cons name ty tailRow row
  , EqualFields tail row
  ) => EqualFields (Cons name ty tail) row where
  equalFields _ a b = get' a == get' b && equalRest a b
    where
      get' = get (SProxy :: SProxy name)
      equalRest = equalFields (RLProxy :: RLProxy tail)

instance equalFieldsNil :: EqualFields Nil row where
equalFields _ _ _ = true

equal
  :: forall r rs
   . RowToList r rs
  => EqualFields rs r
  => Record r
  -> Record r
  -> Boolean
equal a b = equalFields (RLProxy :: RLProxy rs) a b
```
- check two Records of the same type for equality
- `rs :: RowList` here is to enumerate all the field names
- given a field name, `Symbol`, we can `get` its field value

- `get :: forall r r' l a. IsSymbol l => Cons l a r' r => SProxy l -> { | r } -> a`
- `set :: forall r1 r2 r l a b. IsSymbol l => Cons l a r r1 => Cons l b r r2 => SProxy l -> b -> { | r1 } -> { | r2 }`
- `modify :: forall r1 r2 r l a b. IsSymbol l => Cons l a r r1 => Cons l b r r2 => SProxy l -> (a -> b) -> { | r1 } -> { | r2 }`
- `insert :: forall r1 r2 l a. IsSymbol l => Lacks l r1 => Cons l a r1 r2 => SProxy l -> a -> { | r1 } -> { | r2 }`
- `delete :: forall r1 r2 l a. IsSymbol l => Lacks l r1 => Cons l a r1 r2 => SProxy l -> { | r2 } -> { | r1 }`
- `rename :: forall prev next ty input inter output. IsSymbol prev => IsSymbol next => Cons prev ty inter input => Lacks prev inter => Cons next ty inter output => Lacks next inter => SProxy prev -> SProxy next -> { | input } -> { | output }`
- `equal :: forall r rs. RowToList r rs => EqualFields rs r => { | r } -> { | r } -> Boolean`
- `merge :: forall r1 r2 r3 r4. Union r1 r2 r3 => Nub r3 r4 => { | r1 } -> { | r2 } -> { | r4 }`
- `union :: forall r1 r2 r3. Union r1 r2 r3 => { | r1 } -> { | r2 } -> { | r3 }`
- `disjointUnion :: forall r1 r2 r3. Union r1 r2 r3 => Nub r3 r3 => { | r1 } -> { | r2 } -> { | r3 }`
- `nub :: forall r1 r2. Nub r1 r2 => { | r1 } -> { | r2 }`

## Record.Builder (purescript-record)
`newtype Builder a b = Builder(a -> b)`

## Record.ST (purescript-record)
`data STRecord :: Region -> # Type -> Type`

## Data.Record.Fold (purescript-record-fold)

## Global (purescript-globals)
Parse `String` into `Int`/`Number` (helpful because we don't have type-level number and arithmetic yet)
- `readInt :: Int -> String -> Number` ~ `parseInt`
- `readFloat :: String -> Number` ~ `parseFloat`

- `nan :: Number` ~ `NaN`
- `isNaN :: Nubmer -> Boolean` ~ `isNaN`
- `infinity :: Number` ~ `Infinity`
- `isFinite :: Number -> Boolean` ~ `isFinite`

# Mapping Example

## Example 1: Product - Record vs Multi-Argument Constructor
```purescript
data State = State
  { seed :: Int
  , value :: Int
  , iteration :: Int
  }
```
is equivalent to
```purescript
Constructor "State" (Product (Constructor "seed" (Argument Int))
                             (Product (Constructor "value" (Argument Int))
                                      (Constructor "iteration" (Argument Int))
                             )
                    )
```

```purescript
data Position = Position Number Number Number
```
is equivalent to
```purescript
Constructor "Position" (Product (Argument Number)
                                (Product (Argument Number)
                                         (Argument Number)
                                )
                       )
```


## Example 2: Sum - Union vs Enum

```purescript
data Query
  = Init
  | OnKeyDown KeyboardEvent
  | SearchInput String Date
```
is equivalent to
```purescript
Sum (Inl (Constructor "Init" NoArguments))
    (Inr (Sum (Inl (Constructor "OnKeyDown" (Argument KeyboardEvent)))
              (Inr (Constructor "SearchInput" (Product (Argument String)
                                                       (Argument Date)
                                              )
                   )
              )
         )
    )
```

```purescript
data Status
  = Idle
  | Playing
  | Paused
```
is equivalent to
```purescript
Sum (Inl (Constructor "Idle" NoArguments))
    (Inr (Sum (Inl (Constructor "Playing" NoArguments))
              (Inr (Constructor "Paused" NoArguments))
         )
    )
```

# Reference

### 1.[Generic Deriving of Generic Traversals](https://arxiv.org/pdf/1805.06798.pdf)
