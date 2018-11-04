let memory = {}

function memorize( f ) {
  return function ( x ) {
    if ( memory( x ) ) {
      return memory( x )
    } else {
      return f( x )
    }
  }
}
