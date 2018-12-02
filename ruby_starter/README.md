`ruby ./syntax.rb text.txt`

- ruby_sinatra_starter
- ruby_rails_starter
  - `/purchases`
- ruby_rails_api_only
  - `/users`

```javascript
fetch( "/purchases" )
.then( response => response.json() )
.then( data => { console.log(data) } )
```

I use Row Kind to denote the Types of Ruby Hashes.
- exception: Ruby Hashes accept any Type as a key/index.

`ActionController::API` exposes `params` to hold parsed HTTP request parameters.
- `params.require :: ActionController::Parameters ~> Symbol -> ActionController::Parameters`
- `params.permit`
```
:: ActionController::Parameters
~> (Symbol[, Symbol[, ...]]) | Hash
-> ActionController::Parameters { permitted :: Boolean }
```

`before_action :: (Unit -> Nil) -> { only: Array Symbol } -> Nil`
- `Unit -> Nil`, a callback function (`Effect Unit`) which will be executed before the execution of a list of specified functions
- `Array Symbol`, an array of function names (encoded as `Symbol`s)

`wrap_parameters :: { format: Array Symbol } -> Nil`
- `Array Symbol`, an array of formats
  - `:json`, `:xml`, `:url_encoded_form`, `:multipart_form`
- instead of wrapping parameters by entity name in request body, e.g. `user`, now it's valid to just send the parameters
  - `{ user: { name: "wenbo" } }` -> `{ name: "wenbo" }`
