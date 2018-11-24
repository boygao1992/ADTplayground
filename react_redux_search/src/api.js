// @flow
import 'whatwg-fetch'
import type { Gem } from './reducers/gems'

const baseUrl = "http://localhost:3001/api/v1/search.json?query="

export const fetchGems = (search : string) : Promise<Array<Gem>> => {
  return fetch(`${baseUrl}${search}`)
  .then( response => response.json() )
}
