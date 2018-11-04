const Any = function () {
  /*
    type Any =
      { value :: Boolean
      , type :: "Any" }
   */
  const _type = "Any"

  function Any( value ) {
    this.type = _type
    this.value = value
  }
  const create = ( value ) => ( new Any( value ) )
  const mempty = Any( false )
  const mappend = ( m1, m2 ) => {
    if ( ( m1.type && m1.type === _type ) && ( m2.type && m2.type === _type ) ) {
      const { value: b1 } = m1
      const { value: b2 } = m2
      return create( b1 || b2 )
    }
    // throw new Error( "Any.mappend: invalid input data-type" )
    return m1
  }
  return { Any: create, mempty, mappend }
}()

module.exports = Any
