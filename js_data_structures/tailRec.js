const Either = require( "./Either" )

/*
  tailRec :: (a -> Either b a) -> a -> b
  tailRec f i = go (f i)
  where
  go :: Either b a -> b
  go (Right a) = go (f a)
  go (Left b) = b
*/

function tailRec( f, i ) {
  let state = f( i )

  while ( Either.isRight( state ) ) {
    state = Either.bind( f )( state )
  }

  return Either.value( state )
}

module.exports = tailRec
