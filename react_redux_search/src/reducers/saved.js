// @flow

import type { Reducer } from 'redux'

import type { GemName, Gem } from '../../shared/types'

export type State = Array<Gem>

export const initialState : State = []

export type Action$AddGemToSaved =
  {| type: "saved_AddGemToSaved", gem: Gem |}
export type Action$RemoveGemFromSaved =
  {| type : "saved_RemoveGemFromSaved", name: GemName |}
export type Action$FetchSavedGemsSuccess =
  {| type : "saved_FetchSavedGemsSuccess", gems : Array<Gem> |}

export type Action
  = Action$AddGemToSaved
  | Action$RemoveGemFromSaved
  | Action$FetchSavedGemsSuccess


export const addGemToSaved = (gem : Gem) : Action$AddGemToSaved =>
  ({ type: "saved_AddGemToSaved", gem })

export const removeGemFromSaved = (name : string) : Action$RemoveGemFromSaved =>
  ({ type: "saved_RemoveGemFromSaved", name })

export const fetchSavedGemsSuccess = (gems : Array<Gem>): Action$FetchSavedGemsSuccess =>
  ({ type : "saved_FetchSavedGemsSuccess", gems })

export const reducer : Reducer<State, Action> = (state : State = initialState, action : Action) : State => {
  switch (action.type) {
    case "saved_AddGemToSaved":
      let { gem } = action
      return [...state, gem]
    case "saved_RemoveGemFromSaved":
      let { name } = action
      return state.filter( gem => gem.name !== name )
    case "saved_FetchSavedGemsSuccess":
      let { gems } = action
      return gems.map( gem => ({...gem, saved: true}))
    default:
      return state
  }
}
