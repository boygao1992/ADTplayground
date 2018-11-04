const Either = ( function () {
  // Constructors
  function Left( a ) {
    return { type: "Left", value: a }
  }

  function Right( b ) {
    return { type: "Right", value: b }
  }
  // Predicates
  function isLeft( e ) {
    return e.type ? e.type === "Left" : false
  }

  function isRight( e ) {
    return e.type ? e.type === "Right" : false
  }

  // Functor
  function fmap( f ) {
    return e => e.type === "Right" ? Right( f( e.value ) ) : e
  }

  // Bifunctor
  function bimap( f, g ) {
    return e => e.type === "Left" ? Left( f( e.value ) ) : Right( g( e.value ) )
  }

  // Bind
  function bind( k ) {
    return e => e.type === "Right" ? k( e.value ) : e
  }

  function either( f, g ) {
    return e => e.type === "Left" ? f( e.value ) : g( e.value )
  }

  function value( e ) {
    return e.value
  }

  return { Left, Right, isLeft, isRight, fmap, bimap, bind, either, value }
} )()

module.exports = Either
