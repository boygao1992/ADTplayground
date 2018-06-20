const log = require( './lib/log' )

const {
  // ADT
  Pair,
  // Pair
  branch,
  fanout,
  merge,
  // Helper
  curry,
  flip,
  // Combinator
  compose,
  // Point-free
  reduce,
} = require( 'crocks' )

const {
  // Combinator
  converge,
  // Helper
  add, // Number -> Number -> Number
  divide, // Number -> Number -> Number
  length, // [ Number ] -> Number
} = require( 'ramda' )

const data = [ 45, 89, 75, 65 ]

/* solution 1: fully imperative */

// average_1 :: [ Number ] -> Number
function average_1( list ) {
  let sum = 0

  for ( let i = 0; i < list.length; i++ ) {
    sum += list[ i ]
  }

  return sum / list.length
}

/* solution 2: imperative but partially functional*/

// average_2 :: [ Number ] -> Number
function average_2( list ) {
  const sum = list.reduce( add, 0 )
  return sum / list.length
}

/* solution 3: partially point-free with partial application */

// sum :: [ Number ] -> Number
const sum = reduce( add, 0 )

// DivideByLength :: [ Number ] -> Number -> Number
const DivideByLength = curry(
  compose(
    flip( divide ),
    length
  )
)
// average_3 :: [ Number ] -> Number
const average_3 = list =>
  compose( DivideByLength( list ), sum )( list )

/* solution 4: fully point-free with Pair ADT */

// average_4 :: [ Number ] -> Number
const average_4 = compose(
  merge( divide ),
  fanout( sum, length ) // fanout = ( f, g ) => compose( bimap( f, g ), branch )
)

/* solution 5: fully point-free with Ramda converge */

// average_5 :: [ Number ] -> Number
const average_5 = converge(
  divide, [ sum, length ]
)

log( [
  average_1( data ),
  average_2( data ),
  average_3( data ),
  average_4( data ),
  average_5( data ),
] )
