// @flow

import type { Reducer } from 'redux'
export type State = string
export const initialState : State = ""


export type Action$NewSearch =
  {| type : "searching_NewSearch", search : string |}

export const newSearch = (search : string) : Action$NewSearch =>
  ({ type : "searching_NewSearch", search })

export type Action = Action$NewSearch

export const reducer : Reducer<State, Action> = (state : State = initialState, action : Action) : State => {
  switch (action.type) {
    case "searching_NewSearch":
      let { search } = action
      return search
    default:
      return state
  }
}
