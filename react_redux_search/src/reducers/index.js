// @flow

import type { Action as Action$input
            , State as State$input
            } from './input'
import { updateInput
       , reducer as reducer$input
       } from './input'
import type { Action as Action$searching
            , State as State$searching
            } from './searching'
import { newSearch
       , reducer as reducer$searching
       } from './searching'

import type { Action as Action$gems
            , State as State$gems
            } from './gems'
import { fetchSuccess
       , fetchFailure
       , toggleSave
       , reducer as reducer$gems
       } from './gems'

import type { Reducer } from 'redux'
import { combineReducers } from 'redux'

export type State =
  { input : State$input
  , searching : State$searching
  , gems : State$gems
  }

export type Action
  = Action$input
  | Action$searching
  | Action$gems

export const actionCreators =
  { updateInput
  , newSearch
  , fetchSuccess
  , fetchFailure
  , toggleSave
  }

export type ReduxDispatch = Action => void

export type GetState = () => State
export type Thunk = (ReduxDispatch, GetState)  => (Promise<void> | void)
export type ThunkDispatch = Thunk => void

export type Dispatch
  = (Action | Thunk) => void


const reducer : Reducer<State, Action> =
  combineReducers(
    { input : reducer$input
    , searching : reducer$searching
    , gems : reducer$gems
    }
  )

export default reducer
