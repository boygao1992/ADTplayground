// @flow

import { fetchGems
       , saveGem
       , removeGem
       , fetchSavedGems
       } from './api'

import type { Thunk } from './reducers'
import { actionCreators } from './reducers'

const { updateInput
      , newSearch
      , fetchSuccess
      , fetchFailure
      , toggleSave
      , addGemToSaved
      , removeGemFromSaved
      , fetchSavedGemsSuccess
      } = actionCreators

export const fetchGemsThunk = (input : string) : Thunk => {
  return  (dispatch, getState) => {
    dispatch(updateInput(input))
    dispatch(newSearch(input))
    console.log("fetchGemsThunk: fetching")
    return fetchGems(input)
    .then( gems => {
      console.log("fetchGemsThunk: success")
      const { searching } = getState()
      if (input === searching) {
        dispatch(fetchSuccess(gems))
      } else {
        console.log("fetchGemsThunk: cancelled, " + input)
      }
    })
    .catch ( (error : string) => {
      console.log(`fetchGemsThunk: failure, ${error}`)
      const { searching } = getState()
      if (input === searching) {
        dispatch(fetchFailure(error))
      }
    })
  }
}

export const saveGemThunk = (name : string) : Thunk => {
  return  (dispatch, getState) => {
    const saved = getState().saved.filter( gem => gem.name === name).length > 0
    dispatch(toggleSave(name)) // TODO catch failure; should not allow toggling during saving
    if (saved) {
      dispatch(removeGemFromSaved(name))
      return removeGem(name)
      .then( (res) => { console.log(`${res.name}: ${res.status}`) })
      .catch( ({ error }) => { console.log(`error: ${error}`) })
    } else {
      const candidates = getState().gems.filter( gem => gem.name === name)
      if (candidates.length > 0) {
        const gem = candidates[0] // TODO: unsafe
        dispatch(addGemToSaved(gem))
        return saveGem(name)
          .then( (res) => { console.log(`${res.name}: ${res.status}`) })
          .catch( ({ error }) => { console.log(`error: ${error}`) })
      }
    }
  }
}

export const fetchSavedGemsThunk = () : Thunk => {
  return  (dispatch, getState) => {
    return fetchSavedGems()
    .then(gems => {
      dispatch(fetchSavedGemsSuccess(gems))
    })
    .catch(error => {
      console.log(`error: ${error}`)
    })
  }
}
