// @flow
import React from 'react'
import type { Item$State, Item$Input } from './item'

type List$State =
  Array <
    { id : number
    , ...Item$State
    }
  >

  // Input :: Unit
  // Output :: Void

  // Child Slot
  // Child Query

type List$Input$ADD_TODO = { type: "ADD_TODO", id: number, text: string }
const add_todo = ( id: number, text: string ): List$Input$ADD_TODO =>
  ( { type: "ADD_TODO", id, text } )

type List$Input$TOGGLE_TODO = { type: "TOGGLE_TODO", id: number }
const toggle_todo = ( id: number ): List$Input$TOGGLE_TODO =>
  ( { type: "TOGGLE_TODO", id } )

type List$Input
  = List$Input$ADD_TODO
  | List$Input$TOGGLE_TODO

const reducer = ( state: List$State = [], action: List$Input ): List$State => {
  switch ( action.type ) {
    case "ADD_TODO":
      return [ ...state,
        { id: action.id, text: action.text, completed: false }
      ]
    case "TOGGLE_TODO":
      return state.map( item => (
        item.id == action.id ? { ...item, completed: !item.completed } :
        item ) )
    default:
      return state
  }
}
