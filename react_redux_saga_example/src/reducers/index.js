import { combineReducers } from 'redux'
import * as TYPES from '../types'

const initialState = {
  people: []
}

const handleFetchStarWarsSuccess = ( state, action ) => {
  let { data } = action
  return { ...state, people: data }
}

const handlers = {
  [ TYPES.FETCH_STAR_WARS_SUCCESS ]: handleFetchStarWarsSuccess
}

const starwars = ( state = initialState, action ) => {
  return handlers[ action.type ] ?
    handlers[ action.type ]( state, action ) :
    state
}

const rootReducer = combineReducers( {
  starwars
} )

export default rootReducer
