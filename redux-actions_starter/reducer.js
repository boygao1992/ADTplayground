import { handleActions, combineActions } from 'redux-actions'

import {
  UPDATE_CURRENT,
  ADD_TODO,
  LOAD_TODOS,
  REPLACE_TODO,
  REMOVE_TODO,
  SHOW_LOADER,
  HIDE_LOADER,
} from './types'

const initialState = {
  todos: [],
  currentTodo: '',
  isLoading: true,
  message: ''
}

// export default ( state = initialState, action ) => {
//   switch ( action.type ) {
//     case ADD_TODO:
//       return {
//         ...state,
//         currentTodo: '',
//         todos: state.todos.concat( action.payload )
//       }
//     case LOAD_TODOS:
//       return { ...state, todos: action.payload }
//     case UPDATE_CURRENT:
//       return { ...state, currentTodo: action.payload }
//     case REPLACE_TODO:
//       return {
//         ...state,
//         todos: state.todos.map(
//           t => ( t.id === action.payload.id ? action.payload : t )
//         )
//       }
//     case REMOVE_TODO:
//       return {
//         ...state,
//         todos: state.todos.filter( t => t.id !== action.payload )
//       }
//     case SHOW_LOADER:
//     case HIDE_LOADER:
//       return { ...state, isLoading: action.payload }
//     default:
//       return state
//   }
// }

export default handleActions( {
  [ ADD_TODO ]: {
    next: ( state, action ) => ( {
      ...state,
      currentTodo: '',
      todos: state.todos.concat( action.payload )
    } ),
    throw: ( state, action ) => ( {
      ...state,
      message: `error message: ${action.payload.message}, content: ${action.meta.name}`
    } )
  },

  [ LOAD_TODOS ]: ( state, action ) => ( { ...state, todos: action.payload } ),
  [ UPDATE_CURRENT ]: ( state, action ) => ( { ...state, currentTodo: action.payload } ),
  [ REPLACE_TODO ]: ( state, action ) => ( {
    ...state,
    todos: state.todos.map(
      t => ( t.id === action.payload.id ? action.payload : t )
    )
  } ),
  [ REMOVE_TODO ]: ( state, action ) => ( {
    ...state,
    todos: state.todos.filter( t => t.id !== action.payload )
  } ),

  [ combineActions( SHOW_LOADER, HIDE_LOADER ) ]: ( state, action ) => ( { ...state, isLoading: action.payload } )
}, initialState )
