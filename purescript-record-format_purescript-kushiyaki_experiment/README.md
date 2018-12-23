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

- `kind`, the type of type, grouping a set of types together
  - if type is type-level value, then kind is type-level type
  - `Type`, a built-in `kind` from compiler which groups primitive types and any types constructed through type constructor (`data`)
    - primitive-types: `Boolean`, `Int`, `Number`, `Char`, `String`
  - `Symbol`, another built-in `kind` for type-level strings
    - we can use `data SProxy (sym :: Symbol) :: Symbol -> Type` to project/unify `kind Symbol` into `kind Type` which the type system can work with during type inference
  - `# Type`, Row `kind`
  - `->`, Arrow `kind`
  - `Foreign import kind`
    - keyword to construct user-define `kind`
    - define proxy similar to `SProxy` to project it into `Type`

- `class`, type class
    

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

