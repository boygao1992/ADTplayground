const log = require( './lib/log' )
const { readFile, writeFile } = require( 'fs' )

const {
  // ADT
  Async,
  Async: { fromNode },
  // Result
  tryCatch,
  // Natural Transformations
  resultToAsync,
  // Helpers
  constant,
  curry,
} = require( 'crocks' )

// Async fork reflection
const fork = a => a.fork(
  log.bind( null, 'rej' ),
  log.bind( null, 'res' )
)

const parse = tryCatch( JSON.parse )
const readAsync = fromNode( readFile )
const writeAsync = fromNode( writeFile )

// readJSON :: String -> Async Error a
const readJSON = file =>
  readAsync( file, 'utf8' )
  .chain( resultToAsync( parse ) )

// writeJSON :: String -> a -> Async Error a
const writeJSON = curry(
  ( file, data ) =>
  writeAsync( file, JSON.stringify( data, null, 2 ) )
  .map( constant( data ) )
)

module.exports = {
  fork,
  readJSON,
  writeJSON
}
