const log = require( './lib/log' )

const {
  // ADT
  Async,
  List,
  // Helper
  mapReduce, // Foldable f => (a -> b) -> (c -> b -> c) -> c -> f a -> c
} = require( 'crocks' )


const {
  loadTextFile,
  concatResults,
  fork
} = require( './funcs' )

const { fileList: data } = require( './data' )

// [ String ] -> Async (Pair (List String) Error) (List String)
const flow = mapReduce(
  // map :: [ String ] -> [ Async Error String ]
  loadTextFile,
  // reduce :: (Async, Async) -> Async
  concatResults,
  Async.Resolved( List.empty() )
)

fork( flow( data ) )
