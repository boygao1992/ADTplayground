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
