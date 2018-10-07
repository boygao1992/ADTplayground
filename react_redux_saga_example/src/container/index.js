import { connect } from 'react-redux'
import StarWars from '../components/StarWars'
import { fetchStarWarsRequest } from '../actions'

const mapStateToProps = ( { starwars } ) => ( { starwars } )

const bindActionsToDispatch = ( dispatch ) => ( {
  fetchStarWarsRequest: () => dispatch( fetchStarWarsRequest() )
} )

const Container = connect( mapStateToProps, bindActionsToDispatch )( StarWars )

export default Container
