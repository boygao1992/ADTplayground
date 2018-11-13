import {
  loadTodos,
  addTodo,
  replaceTodo,
  removeTodo,
  showLoader,
  hideLoader,
} from './types'

import {
  getTodos,
  createTodo,
  updateTodo,
  destroyTodo,
} from './API'

export const fetchTodos = () => {
  return dispatch => {
    dispatch( showLoader() )
    getTodos()
      .then( todos => {
        dispatch( loadTodos( todos ) )
        dispatch( hideLoader() )
      } )
  }
}

export const saveTodo = name => {
  return dispatch => {
    dispatch( showLoader() )
    createTodo( name )
      .then( res => {
        dispatch( addTodo( res ) )
        dispatch( hideLoader() )
      } )
      .catch( err => {
        dispatch( addTodo( err, name ) )
        dispatch( hideLoader() )
      } )
  }
}

export const toggleTodo = id => {
  return ( dispatch, getState ) => {
    dispatch( showLoader() )
    const { todos } = getState()
    const todo = todos.find( t => t.id === id )
    const toggled = { ...todo, isComplete: !todo.isComplete }
    updateTodo( toggled )
      .then( res => {
        dispatch( replaceTodo( res ) )
        dispatch( hideLoader() )
      } )
  }
}

export const deleteTodo = id => {
  return dispatch => {
    dispatch( showLoader() )
    destroyTodo( id )
      .then( () => {
        dispatch( removeTodo( id ) )
        dispatch( hideLoader() )
      } )
  }
}

export const getVisibleTodos = ( todos, filter ) => {
  switch ( filter ) {
    case 'active':
      return todos.filter( t => !t.isComplete )
    case 'completed':
      return todos.filter( t => t.isComplete )
    default:
      return todos
  }
}
