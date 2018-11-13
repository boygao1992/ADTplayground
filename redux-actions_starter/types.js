import { createActions } from 'redux-actions'

export const UPDATE_CURRENT = 'UPDATE_CURRENT'
export const ADD_TODO = 'ADD_TODO'
export const LOAD_TODOS = 'LOAD_TODOS'
export const REPLACE_TODO = 'REPLACE_TODO'
export const REMOVE_TODO = 'REMOVE_TODO'
export const SHOW_LOADER = 'SHOW_LOADER'
export const HIDE_LOADER = 'HIDE_LOADER'

/*
 data FlexStandardAction =
   { type :: String
   , payload :: Any
   , error :: Boolean
   , meta :: Any
   }
 */
// export const updateCurrent = val => ({ type: UPDATE_CURRENT, payload: val })
// export const loadTodos = todos => ( { type: LOAD_TODOS, payload: todos } )
// export const addTodo = todo => ( { type: ADD_TODO, payload: todo } )
// export const replaceTodo = todo => ( { type: REPLACE_TODO, payload: todo } )
// export const removeTodo = id => ( { type: REMOVE_TODO, payload: id } )
// export const showLoader = () => ( { type: SHOW_LOADER, payload: true } )
// export const hideLoader = () => ( { type: HIDE_LOADER, payload: false } )

// export const updateCurrent = createAction( UPDATE_CURRENT )
// export const loadTodos = createAction( LOAD_TODOS )
// export const addTodo = createAction( ADD_TODO )
// export const replaceTodo = createAction( REPLACE_TODO )
// export const removeTodo = createAction( REMOVE_TODO )
// export const showLoader = createAction( SHOW_LOADER, () => true )
// export const hideLoader = createAction( HIDE_LOADER, () => false )

const identity = x => x

export const {
  updateCurrent,
  loadTodos,
  addTodo,
  replaceTodo,
  removeTodo,
  showLoader,
  hideLoader,
} = createActions( {
    [ SHOW_LOADER ]: () => true,
    [ HIDE_LOADER ]: () => false,
    [ ADD_TODO ]: [
      identity, // payload
      ( payload, name ) => ( { name } ) // meta
    ],
  },
  UPDATE_CURRENT,
  LOAD_TODOS,
  REPLACE_TODO,
  REMOVE_TODO,
)
