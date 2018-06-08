const fs = require( 'fs' )

const log = require( './lib/log' )

const {
  // ADT
  Async,
  List,
  Pair,
  // Maybe
  safe, // (a -> Boolean) -> a -> Maybe b
  // Natural Transformation
  maybeToAsync, // e -> Maybe a -> Async e a
  // Helper
  binary, // (a -> b -> c) -> (a,b) -> c
  compose, // ((y -> z), ..., (a -> b)) -> a -> z
  composeK, // Monad m => ((y -> m z), ..., (a -> mb)) -> a -> mz
  constant,
  curry, // ((a,b,...) -> z) -> a -> b -> ... -> z
  fanout, // (a -> b) -> (a -> c) -> (a -> Pair b c)
  flip, // (a -> b -> c) -> b -> a -> c
  identity, // a -> a
  // Pointfree
  bimap, // Bifunctor m => (a -> c) -> (b -> d) -> m a b -> m c d
  concat, // Semigroup m => m a -> m a -> m a
  chain, // Monad m => (a -> m b) -> m a -> m b
} = require( 'crocks' )

const { fromNode } = Async

const access = fromNode( fs.access )
const readFile = fromNode( fs.readFile )

// accessAsync :: mode -> a -> Async Error b
const accessAsync = curry(
  ( mode, path ) =>
  access( path, mode )
  .map( constant( path ) )
)

// readFileAsync :: Options -> a -> Async Error b
const readFileAsync = curry(
  ( options, path ) =>
  readFile( path, options )
)

// reflection
const fork = a => a.fork(
  log.bind( null, 'rej' ),
  log.bind( null, 'res' )
)

// checkRead :: String -> Async Error String
const checkRead = accessAsync( fs.constants.F_OK )

// readTextFile :: String -> Async Error String
const readTextFile = readFileAsync( 'utf-8' )

// loadTextFile :: String -> Async Error String
const loadTextFile = composeK(
  readTextFile,
  checkRead
)

// List String -> ( String -> List String )
const concatListOf = xs =>
  compose(
    flip( concat, xs ),
    List.of
  )

// Async (Pair (List String) Error) (List String) -> (List String) -> Async
const concatListOfAsync = flip(
  // [ String ] -> ( Async Error String -> Async (Pair [ String ] Error) [ String ])
  xs => bimap(
    // Error -> Pair [ String ] Error
    fanout( constant( xs ), identity ), // e => Pair( xs, e )
    // String -> List String
    concatListOf( xs )
  )
)


// ( Async (Pair (List String) Error) (List String), Async Error String )
//  -> Async (Pair (List String) Error) (List String)
const concatResults = binary(
  // Async (Pair (List String) Error) (List String) -> Async Error String
  //  -> Async (Pair (List String) Error) (List String)
  flip(
    // Async Error String -> Async (Pair (List String) Error) (List String)
    //  -> Async (Pair (List String) Error) (List String)
    x => chain(
      // List String -> Async (Pair (List String) Error) (List String)
      concatListOfAsync( x )
    )
  )
)


module.exports = {
  loadTextFile,
  concatResults,
  fork,
}
