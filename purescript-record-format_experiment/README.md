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
