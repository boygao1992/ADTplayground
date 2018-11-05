/* global d3 */

// d3.scaleLinear :: Tuple Number Number -> Tuple Number Number -> Number -> Number
// linearScale :: Number -> Number
// ~ linearScale x = x / 100.0
// e.g.
//   linearScale(-1) = -0.01
//   linearScale(0) = 0
//   linearScale(100) = 1
const linearScale = d3.scaleLinear()
  .domain( [ 0, 100 ] )
  .range( [ 0, 0 ] )

// d3.scaleTime :: Tuple Date Date -> Tuple Number Number -> Date -> Number
const scaleTime = d3.scaleTime()
  .domain( [ new Date( 2016, 0, 1 ), new Date() ] )
  .range( [ 0, 100 ] )

// timeScale :: Number -> Date
const timeScale = scaleTime.invert

// d3.scaleQuantize :: forall a. Tuple Number Number -> Array a -> Number -> a
//   from a continuous domain to a discrete range
// scaleQuantize :: Number -> a
// scaleQuantize x =
//   x in ( -infinity, 100/3 )      -> "aqua"
//   x in [     100/3, 200/3 )      -> "blue"
//   x in [     200/3, + infinity ) -> "charcoal"
const scaleQuantize = d3.scaleQuantize()
  .domain( [ 0, 100 ] )
  .range( [ "aqua", "blue", "charcoal" ] )

// quantizeScale :: a -> Tuple Number Number
// quantizeScale a
//   | a == "aqua"     = [     0, 100/3 ]
//   | a == "blue"     = [ 100/3, 200/3 ]
//   | a == "charcoal" = [ 200/3,   100 ]
const quantizeScale = scaleQuantize.invertExtent

// d3.scaleOrdinal :: forall a b. Array a -> Array b -> a -> b
//   from a discrete domain to a discrete range
// edge cases:
// 1. length of (Array a) is equal to the length of (Array b)
//   - any input not in (Array a) will be mapped to the 1st element of (Array b)
// 2. length of (Array a), n, is larger than the length of (Array b), m
//   - any input not in (Array a) will be mapped to the (n+1)th element of (Array b)
// 3. length of (Array a), n, is less than the length of (Array b), m
//   - (i)th element of (Array a) will be mapped to the (i % m)th element of (Array b)
//   - any input not in (Array a) will be mapped to the last element of (Array b)
// e.g.
//   [ "A", "B", "C", "D", "E" ] -> [ 4, 3 ]
//   scaleOrdinal("E") = 4 // i = 4, m = 2, i % m = 0
const scaleOrdinal = d3.scaleOrdinal()
  .domain( [ "A", "B" ] )
  .range( [ 4, 3 ] )
