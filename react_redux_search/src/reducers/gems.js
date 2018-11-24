// @flow

import type { Reducer } from 'redux'

export type Gem =
  {| name : string
   , authors : Array<string>
   , info : string
   , version : string
   , downloads: number
   , saved : boolean
   |}

export type State = Array<Gem>

export const initialState : State =
  [ { name: "rails"
    , downloads: 150542845
    , version: "5.2.1"
    , authors: ["David Heinemeier Hansson"]
    , info: "Ruby on Rails is a full-stack web framework optimized for programmer happiness and sustainable productivity. It encourages beautiful code by favoring convention over configuration."
    , saved: false
    }
  ]

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
