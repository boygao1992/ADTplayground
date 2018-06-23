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
  identity,
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

// blueprint:
// List(operations) // List Func
// .map( ask ) // List (Reader Number Number)
// .sequence( Reader.of ) // Reader Number (List Number)
// .map( listToArray ) // Reader Number [ Number ]
// .map( ( [ x, y ] ) => divide( x, y ) ) // Reader Number Number
// .runWith( data ) // Number

// converge :: ((x1, x2, …) → z) → [((a, b, …) → x1), ((a, b, …) → x2), …] → (a → b → … → z)
// myConverge :: ((x1, x2, …) → b) → [(a → x1), (a → x2), …] → Reader a b
const myConverge = curry(
  reducer => compose(
    map( reducer ),
    map( listToArray ),
    sequence( Reader.of ),
    map( ask ),
    List,
  )
)

const operations = [
  sum,
  length
]

// dummyReducer :: [ Number, Number ] -> Number
const dummyReducer = ( [ x, y ] ) => divide( x, y )

// average_6 :: Reader [ Number ] Number
const average_6 = myConverge( dummyReducer, operations )

// 0 -> [ 1, 2, 3, 4]
// log(
//   runWith( 0 )(
//     myConverge( identity )( [
//       x => x + 1,
//       x => x + 2,
//       x => x + 3,
//       x => x + 4
//     ] )
//   )
// )

log( [
  average_1( data ),
  average_2( data ),
  average_3( data ),
  average_4( data ),
  average_5( data ),
  runWith( data )( average_6 )
] )
