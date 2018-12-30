# Type-level Arithmetic

## type-level mapping

Cons
- almost no guidance from compiler during implementation
- duck typing through type classes like `IsSymbol`, `IsBoolean`
  - pretty much like programming in JS
- no Effect abstraction
  - popping `TypeError` is an IO Effect
  
```purescript
class rejectFalse (b :: Boolean)

instance rejectFalse True
-- no instance for False, so if `b == False` then the compiler will throw "no type class instance" TypeError
```

## Digit

## Number

## Add
