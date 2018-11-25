// @flow
import 'whatwg-fetch'
import type { FetchGemsResponse
            , SaveGemResponse
            , RemoveGemResponse
            , FetchSavedGemsResponse
            } from '../shared/types'

const baseUrl = "http://localhost:3001"
const fetchGemsUrl = query => `${baseUrl}/api/v1/search.json?query=${query}`
const saveGemUrl = name => `${baseUrl}/gem/add/${name}`
const removeGemUrl = name => `${baseUrl}/gem/remove/${name}`
const fetchSavedGemsUrl = `${baseUrl}/gem`

/* Utils */
const getJson = ( url : string ) : Promise<*> => fetch(url).then( response => response.json() )
const postJson = ( url : string , body : ?Object) : Promise<*> =>
  fetch( url
       , { method: 'POST'
         , headers:
           { 'Accept': 'application/json'
           , 'Content-Type': 'application/json'
           }
         , body
         }
       )
 .then( response => response.json() )

export const fetchGems = (query : string) : Promise<FetchGemsResponse> => {
  return getJson(fetchGemsUrl(query))
}

export const saveGem = (name : string) : Promise<SaveGemResponse> => {
  return postJson(saveGemUrl(name))
}

export const removeGem = (name : string) : Promise<RemoveGemResponse> => {
  return postJson(removeGemUrl(name))
}

export const fetchSavedGems = () : Promise<FetchSavedGemsResponse> => {
  return getJson(fetchSavedGemsUrl)
}
