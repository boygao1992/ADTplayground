const log = require( './lib/log' )
const {
  fork,
  readJSON,
  writeJSON,
} = require( './helpers' )

const {
  // ADTs
  Reader,
  // Maybe
  prop,
  // Reader
  Reader: { ask },
  // Combinators
  composeK,
  // Helpers
  constant,
  liftA2, // Applicative m => (a -> b -> c) -> m a -> m b -> m c
  // Point-free
  map,
  runWith,
} = require( 'crocks' )

const pairUp =
  liftA2( x => y => [ x, y ] )

const data = {
  happy: true,
  sad: false,
}

const happy =
  ask( prop( 'happy' ) )

const sad =
  ask( prop( 'sad' ) )

const flow = composeK(
  h => sad.map( pairUp( h ) ),
  constant( happy )
)

log(
  runWith( data )( flow() )
)
