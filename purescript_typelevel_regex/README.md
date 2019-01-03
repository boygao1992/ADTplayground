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

### Mapping from Data-Type to Kind

#### Product

```purescript
-- | Value Level
--   Pattern :: kind Type
                                 -- Char :: kind Type
                                         -- Int :: kind Type
                                              --   Pattern :: kind Type
                 -- CharNumFixed :: Char -> Int -> Pattern
data Pattern = CharNumFixed Char Int
```
- `CharNumFixed` is a data constructor
  - whose instances are unified under type `Pattern`
- type `Pattern` is a product of type `Char` and type `Int`

```purescript
-- | Type Level
                 -- Pattern :: kind Pattern
foreign import kind Pattern
                                 -- Symbol :: kind Symbol
                                           -- Symbol :: kind Symbol
                                                     -- Pattern :: kind Pattern
foreign import data CharNumFixed :: Symbol -> Symbol -> Pattern
```
- `CharNumFixed` is a type constructor
  - whose instances are unified under kind `Pattern`
- kind `Pattern` is a product of kind `Symbol` and kind `Symbol`

#### Sum

```purescript
-- | Value Level
--   Pattern :: kind Type
data Pattern
  = CharNumFixed Char Int -- CharNumFixed :: Char -> Int -> Pattern
  | CharNumMaybe Char -- CharNumMaybe :: Char -> Pattern
  | CharNumPositive Char -- CharNumPositive :: Char -> Pattern
  | CharStar Char -- CharStar :: Char -> Pattern
```
- `CharNumFixed`, `CharNumMaybe`, `CharNumPositive`, `CharStar` are all data constructors
  - whose instances are all unified under type `Pattern`

```purescript
-- | Type Level
foreign import kind Pattern
foreign import data CharNumFixed :: Symbol -> Symbol -> Pattern
foreign import data CharNumMaybe :: Symbol -> Pattern
foreign import data CharNumPositive :: Symbol -> Pattern
foreign import data CharStar :: Symbol -> Pattern
```
- `CharNumFixed`, `CharNumMaybe`, `CharNumPositive`, `CharStar` are all type constructors
  - whose instances are all unified under kind `Pattern`

### Mapping from Functions to Type Class with Functional Dependency

#### Definition

```purescript
recognize :: Array Pattern -> String -> Boolean
```

```purescript
class Recognize (pl :: PList) (str :: Symbol) (b :: Bool.Boolean) | pl str -> b
```

#### Pattern Matching

```purescript
-- | Value Level
recognizeBaseCaseDispatch :: Pattern -> Array Pattern -> String -> Boolean

recognizeBaseCase :: Array Pattern -> Array Char -> Boolean
recognizeBaseCase pl str =
  if pl == PNil
  then
    if str == ""
    then
      true
    else
      false
  else
    recognizeReversedDispatch p_h p_t str
-- only if recognizeReversedDispatch terminates in finite steps, can recognizeBaseCase have a Boolean as its result (otherwise, bottom)

-- | Type Level
class RecognizeReversedDispatch (p_h :: Pattern) (p_t :: PList) (s :: Symbol) (b :: Bool.Boolean) | p_h p_t s -> b

class RecognizeReversed (pl :: PList) (str :: Symbol) (b :: Bool.Boolean) | pl str -> b
instance recognizeReversedBaseCase1 ::
  RecognizeReversed PNil "" Bool.True
else instance recognizeReversedBaseCase2 ::
  RecognizeReversed PNil str Bool.False
else instance recognizeReversedInductionStep ::
  ( RecognizeReversedDispatch p_h p_t str b
  ) => RecognizeReversed (PCons p_h p_t) str b
-- arrow (=>) means derivation:
--   only if RecognizeReversedDispatch has a matching instance
--   , can RecognizeReversed have a matching instance
-- The functional dependency, pl str -> b
-- , means that, given an instance of PList and an instance of Symbol
-- , we can uniquely infer an instance of Boolean.
```

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
  
