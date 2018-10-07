import { call, put } from 'redux-saga/effects'
import * as TYPES from '../types'

const URL_BASE = "http://localhost:3001"
const URL_PEOPLE = `${URL_BASE}/people`

const api = ( url ) => fetch( url )
  .then( res => res.json() )

export const fetchStarWarsRequest = () => ( { type: TYPES.FETCH_STAR_WARS_REQUEST } )

export function* fetchPerson( action ) {
  try {
    const { results } = yield call( api, URL_PEOPLE )
    yield put( { type: TYPES.FETCH_STAR_WARS_SUCCESS, data: results } )
  } catch ( e ) {
    console.log( e )
  }
}
