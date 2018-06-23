const log = require( './lib/log' )

const {
  // ADT
  List,
  Pair,
  Reader,
  // Reader
  Reader: { ask },
  // Pair
  branch,
  fanout,
  merge,
  // Natural Transformation
  listToArray,
  // Helper
  constant,
  curry,
  flip,
  // Combinator
  compose,
  // Point-free
  map,
  reduce,
  runWith,
  sequence,
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

/* solution 6: traverse a List of Readers */
const operations = List( [
  sum,
  length
] )

// average_6 :: Reader [ Number ] Number
const average_6 = compose(
  map( ( [ x, y ] ) => divide( x, y ) ),
  map( listToArray ),
  sequence( Reader.of ),
  map( ask ),
  constant( operations )
)

// log(
//   operations // List Func
//   .map( ask ) // List (Reader Number Number)
//   .sequence( Reader.of ) // Reader Number (List Number)
//   .map( listToArray ) // Reader Number [ Number ]
//   .map( ( [ x, y ] ) => divide( x, y ) ) // Reader Number Number
//   .runWith( data ) // Number
// )

log( [
  average_1( data ),
  average_2( data ),
  average_3( data ),
  average_4( data ),
  average_5( data ),
  runWith( data )( average_6() )
] )
