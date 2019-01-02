# purescript-typelevel-regex

## Code Organization

- `src/Regex/Runtime.purs`
  - runtime version
- `src/Symbol/Regex.purs`
  - type-level encoding of the runtime version
- `src/Num`
  - type-level arithmetic for qualifier support
  - imported from my another project [`purescript-typelevel-arithmetic`](https://github.com/boygao1992/ADTplayground/tree/master/purescript_typelevel_arithmetic)
- `src/Symbol/Utils.purs`
  - utility functions for type-level literals

## Status

- [ ] Quantifiers
  - [x] `{n}`
  - [ ] `{n,}`
  - [ ] `{n, m}`
  - [x] `?`
  - [x] `+`
  - [x] `*`
- [ ] Special characters
  - [ ] `^`
  - [ ] `$`
  - [x] `.`
  - [ ] `?`
- [x] Literal characters
- [ ] Character sets
- [ ] Escaping `\`
- [ ] Character Escapes
  - [ ] `\d`, short for `[0-9]`
  - [ ] `\D`, short for `[^0-9]`
  - [ ] `\w`, short for `[a-zA-Z0-9_]`
  - [ ] `\W`, short for `[^a-zA-Z0-9_]`

## Usage

- `recognize :: forall regex str pl b. Parse regex pl => Recognize pl str b => SProxy regex -> SProxy str -> BProxy b`
  - lift a regular expression and a matching string into types using `SProxy`
  - will return result in type-level Boolean carried by `BProxy`

```purescript
class RejectFalse (b :: Boolean)

instance rejectFalse :: RejectFalse True
-- no instance for False, so if `b == False` then the compiler will throw "no type class instance" TypeError
```
- `RejectFalse b => Recognize pl str b =>`
  - convert the type-level predicate `Recognize` into a type class constraint this way
  - will polish the library API after some practical usage in other projects

## Examples

```purescript
recognizeExample1 :: BProxy True
recognizeExample1 = recognize
                    (SProxy :: SProxy "aa?bc*de{3}f.?g")
                    (SProxy :: SProxy "abdeeefg")
```

## Type-level Programming Miscellanies

### mapping from data-type to kind

#### Product

#### Sum

### mapping from runtime functions to type class with functional dependency

### current issues

- almost no guidance from compiler during implementation
  - this is why a faithful runtime encoding is extremely helpful
- duck typing through type class constraints like `IsSymbol`, `IsBoolean`
  - pretty much like programming in native JS using `typeof`
- no Effect abstractions
  - popping `TypeError` is an `Except` Effect (possibly the only one though)
    - in my runtime version, this is captured by `Either`
- need to explicitly name all the control logic
  - whenever we want to pattern match on one or more type variables for conditional branching, we need a new type class
  - this is the reason for lengthy naming in my implementation
  
