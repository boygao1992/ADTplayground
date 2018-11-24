// @flow

import { fetchGems } from './api'
import type { Thunk } from './reducers'
import { actionCreators } from './reducers'

const { updateInput
      , newSearch
      , fetchSuccess
      , fetchFailure
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
