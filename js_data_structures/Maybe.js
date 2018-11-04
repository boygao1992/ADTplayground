// Maybe

const Maybe = ( function () {
  function Just( x ) {
    return { type: "Just", value: x }
  }

  const _Nothing = { type: "Nothing" }

  function Nothing() {
    return _Nothing
  }

  function withDefault( d, m ) {
    switch ( m.type ) {
      case "Just":
        return m.value
      case "Nothing":
        return d
      default:
        return null // throw error
    }
  }

  function isJust( m ) {
    return m.type ? m.type === "Just" : false
  }

  function isNothing( m ) {
    return m.type ? m.type === "Nothing" : false
  }

  // Functor
  function fmap( f ) {
    return m => m.type === "Just" ? Just( f( m.value ) ) : m
  }

  // Bind
  function bind( k ) {
    return m => m.type === "Just" ? k( m.value ) : m
  }

  return { Just, Nothing, withDefault, isJust, isNothing, fmap, bind }
} )()

module.exports = Maybe
