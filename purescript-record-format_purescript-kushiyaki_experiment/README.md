# Mapping from value-level functions to type-level functions

Currently, the type-level inferences are centered around Type Class abstraction which is not as expressive as value-level constructs but is powerful enough to encode FSMs.

## Type-level Constructs

- `data`, type constructor
  - simplest type-level functions that map a type to another
    - `data Functor a :: Type -> Type`
  - higher-order functions are allowed through multi-argument constructors
    - `data MultiArg a b c :: Type -> Type -> Type -> Type`
  - `Foreign import data`
    - keyword to construct type constructor for a `kind` different than `kind Type`

- `kind`, the type of type
  - grouping a set of types together through Union/Sum/Coproduct
    - closed polymorphism
      - close-world assumption at compile time
      - at compile time, we can assume all the instances defined under this kind are all possible instances we will encounter at runtime
  - if type is type-level value, then kind is type-level type
  - `Type`, a built-in `kind` from compiler which groups primitive types and any types constructed through type constructor (`data`)
    - primitive-types: `Boolean`, `Int`, `Number`, `Char`, `String`
  - `Function` / `->`, Arrow `kind`
    - no proxy is needed, `a -> b` or `Function a b` is of `kind Type`
      - it will be super cumbersome if each time we construct a function, we need to explicitly convert the type under `kind Arrow` to `kind Type` through a proxy
  - `Symbol`, another built-in `kind` for type-level strings
    - we can use `data SProxy (sym :: Symbol) :: Symbol -> Type` to project/unify `kind Symbol` into `kind Type` which the type system can work with during type inference
  - `# Type`, Row `kind`
    - `Record :: # Type -> Type` (under `Prim`)
      - following the naming convention, this can be called `RProxy`
      - we do have `RProxy` under `Type.Row` (`purescript-typelevel-prelude`)
        - but its data constructor is a simple no-argument constructor `RProxy`
          - so it's purpose is sorely carrying the type
        - while `Record`'s data constructor is richer and contains all the key-value pairs at value level
  - `RowList`, RowList `kind`
    - `RLProxy :: RowList -> Type`
  - `Foreign import kind`
    - keyword to construct user-define `kind`
    - define proxy similar to `SProxy` to project it into `Type`

- `class`, type class
  - single-argument type class
    - grouping a set of types through Union/Sum/Coproduct
      - closed polymorphism
  - multi-argument type class, grouping a set of (Sum) types through Product
    - without functional dependency
      - Cartesian product of multiple sets
    - functional dependency
      - enforce functional constraint on relationship between two sets
        - an element in the input set can only be uniquely mapped to an element in the output set
      - multi-argument function dependencies are allowed
  - function definition under type class
    - essence: two functions of the same "shape"/"content" but of different types are distinct functions
    - we use type inference to derive function instances of different types
      - type class instance is like a template to construct those concrete functions

# Record.Format (purescript-record-format)
With type-level programming arsenal, no compiler support for template literal is needed.
We can craft and customize the parser as we want and directly enrich the language.

`format :: Format string row => SProxy string -> Record row -> String`
- `class Format (string :: Symbol) (row :: # Type)`
  - given a type-level template literal `string :: Symbol`, compiler will uniquely infer the shape of desired `Record`s (`row :: # Type`)
  - if format tokens in the parsed `string` don't exactly match the field names of `row` in the provided `Record row`, then compiler will throw "Could not match type"

Current implementation is not as powerful as JS native template literal
- no escape token, like `\`, for `{` and `}`
- parser is yet too simple to handle cases like below
  - `{ name }` is parsed as ` name `, which is not a valid field name
  - `{{name}}` is parsed as `{name`, which is not a valid field name
  
# Kushiyaki (purescript-kushiyaki)

