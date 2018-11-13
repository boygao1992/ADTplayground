`redux-actions` is not recommended.

It doesn't save much boilerplate but

- complicates the type signatures of action creators
  - ` (Payload) | (Error[, Meta]) -> Action` whose input type is now a tagless union
- `handleActions` 
  - magically converts "UPPER_CASE" literals to "camelCase" named functions which can be error prone
  - error events are now implicitly defined as sub-events of regularly defined events, which is against "events as data" for inspectability and testability

