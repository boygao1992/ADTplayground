const log = require( './lib/log' )
// Intersection <= Monoid
const Intersection = xs => ( {
  xs,
  // xs: this, ys: other
  concat: ( { xs: ys } ) =>
    xs ?
    (
      ys ? Intersection( xs.filter( x => ys.some( y => x === y ) ) ) :
      Intersection( xs )
    ) : Intersection( ys )
} )

Intersection.empty = Intersection()

const A = [ 1, 2, 3, 4, 5 ]
const B = [ 2, 3, 4 ]
const C = [ 3, 4, 5 ]

const list = [ A, B, C ].map( Intersection )
  .reduce( ( xs, x ) => xs.concat( x ), Intersection.empty )

log(list)
