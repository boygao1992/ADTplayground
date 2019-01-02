# Type-level Arithmetic

## Digit

- `class IsDigit (d :: Symbol)`
  - type class constraint for `Symbol` encoding of type-level digits
  - `d \in [0-9]`
- `class IsDigitPred (d :: Symbol) (b :: Bool.Boolean) | d -> b`
  - type-level predicate for `IsDigit`
- `class Compare (x :: Symbol) (y :: Symbol) (o :: Ord.Ordering) | x y -> o`
  - comparison of two Digits
  - can be treated as a type-level instance of `compare` under `Ord` type class
    - `instance ordDigit :: Ord Digit where compare :: Digit -> Digit -> Ordering`
- `class Add (x :: Symbol) (y :: Symbol) (carry :: Symbol) (z :: Symbol) | x y -> carry z`
  - addition of two Digits
    - `x, y, z \in [0-9]`
    - `carry \in {0, 1}`
  - one dependency of `class Nat.Add` and `class Int.Add`
  - can be treated as a type-level instance of `add` under `Semiring` type class, if without carrying digit
    - `instance semiringDigit :: Semiring Digit where add :: Digit -> Digit -> Digit`
- `class Minus (x :: Symbol) (y :: Symbol) (carry :: Symbol) (z :: Symbol) | x y -> carry z`
  - subtraction of two Digits
    - `x, y, z \in [0-9]`
    - `carry \in {0, 1}`
      - `1` denotes `-1` for simplicity
  - one dependency of `class Int.Minus`
  - can be treated as a type-level instance of `sub` under `Ring` type class, if without carrying digit
  
## IsNat

- `class IsNat (n :: Symbol)`
  - type class constraint for `Symbol` encoding of type-level natural numbers
- `class IsNatPred (n :: Symbol) (b :: Bool.Boolean) | n -> b`
  - type-level predicate for `IsNat`
- `class Add (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z`
  - addition of two Nats
- `class Succ (pred :: Symbol) (succ :: Symbol) | pred -> succ`
  - successor operator for Nat

## IsInt

- `class IsSign (sign :: Symbol)`
  - type class constraint for `Symbol` encoding of signs of numbers
  - `sign \in {"+", "-"}`
- `class IsSignPred (sign :: Symbol) (b :: Bool.Boolean) | sign -> b`
  - type-level predicate for `IsSign`
- `class HasSignPred (num :: Symbol) (b :: Bool.Boolean) | num -> b`
  - type-level predicate to check whether a type-level integer has sign
  - assumption: `num :: Symbol` is under `class IsInt`
- `class IsInt (num :: Symbol)`
  - type class constraint for `Symbol` encoding of integers
- `class IsIntPred (num :: Symbol) (b :: Bool.Boolean) | num -> b`
  - type-level predicate for `IsInt`
- `class IsNegativePred (num :: Symbol) (b :: Bool.Boolean) | num -> b`
  - type-level predicate to check whether a type-level integer is negative
  - assumption: `num :: Symbol` is under `class IsInt`
  - preprocessed by `class Normalize`, so `-0` is not mapped to negative
- `class Inverse (x :: Symbol) (inv :: Symbol) | x -> inv`
  - addictive inverse of Int
- `class Compare (x :: Symbol) (y :: Symbol) (o :: Ord.Ordering) | x y -> o`
  - comparison of two Ints
- `class Add (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z`
  - addition of two Ints
- `class Minus (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z`
  - subtraction of two Ints
- `class Normalize (i :: Symbol) (o :: Symbol) | i -> o`
  - normalization step for `Symbol` encoding of type-level integers
    1. remove positive sign (`+`)
      - e.g. `+123` -> `123`
    2. remove prefixing zeros
      - e.g. `-0000` -> `-0`
    3. remove negative sign of zero if exists
      - e.g. `-0` -> `0`
- `class Pred (num :: Symbol) (pred :: Symbol) | num -> pred`
  - predecessor operator for Int


