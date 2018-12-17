PureScript compiler Generic support
[purescript/src/Language/PureScript/Sugar/TypeClasses/Deriving.hs](https://github.com/purescript/purescript/blob/master/src/Language/PureScript/Sugar/TypeClasses/Deriving.hs)

Algebraic composition can be generalized into two binary operators on types
- Sum
- Product

which can be further generalized as Tensor Product in Monoidal Category.

Terminals
- `NoConstructors :: Type`
- `NoArguments :: Type`
- `name :: Symbol`

Non-terminals
- `Argument :: Type -> Type`
- `Constructor :: Symbol -> Type -> Type`
- `Sum :: Type -> Type -> Type`
- `Product :: Type -> Type -> Type`

Grammer
- `a -> Argument a`, where `a :: Type`
- `name, Argument a -> Constructor name (Argument a)`
- `(NoConstructors | Constructor name1 (Argument a)),
(NoConstructors | Constructor name2 (Arbument b))
-> Sum (...) (...)`
- `(NoArguments | Argument a), (NoArguments | Argument b) -> Product (...) (...)`

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
