// @flow

import type { Reducer } from 'redux'

import type { Gem } from '../../shared/types'

export type State = Array<Gem>

export const initialState : State = []

export type Action$FetchSuccess =
  {| type: "gems_FetchSuccess", gems : Array<Gem> |}
export type Action$FetchFailure =
  {| type: "gems_FetchFailure", error : string |}
export type Action$ToggleSave =
  {| type: "gems_ToggleSave", name : string |}

export type Action
  = Action$FetchSuccess
  | Action$FetchFailure
  | Action$ToggleSave

export const fetchSuccess = (gems : Array<Gem>): Action$FetchSuccess =>
  ({ type: "gems_FetchSuccess", gems })
export const fetchFailure = (error : string) : Action$FetchFailure =>
  ({ type: "gems_FetchFailure", error })
export const toggleSave = (name : string): Action$ToggleSave =>
  ({ type: "gems_ToggleSave", name })

export const reducer : Reducer<State, Action> = (state : State = initialState, action : Action) : State => {
  switch (action.type) {
    case "gems_FetchSuccess":
      let { gems } = action
      return gems
    case "gems_ToggleSave":
      let { name } = action
      return state
             .map( gem =>
               gem.name === name
               ? {...gem, saved : !gem.saved}
               : gem )
    default:
      return state
  }
}
