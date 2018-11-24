// @flow

import type { Reducer } from 'redux'

export type State = string

export const initialState : State = ""

export type Action$UpdateInput =
  {| type : "input_UpdateInput", input : string |}

export const updateInput = (input : string) : Action$UpdateInput =>
  ({ type : "input_UpdateInput", input })

export type Action = Action$UpdateInput

export const reducer : Reducer<State, Action> = (state : State = initialState, action : Action) : State => {
  switch (action.type) {
    case "input_UpdateInput":
      let { input } = action
      return input
    default:
      return state
  }
}
