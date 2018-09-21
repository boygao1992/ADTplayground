// a dummy reducer
const COUNTER_INCREMENT = "increment"
const COUNTER_DECREMENT = "decrement"
const counter = ( state = 0, action ) => {
  switch ( action.type ) {
    case COUNTER_INCREMENT:
      return state + 1
    case COUNTER_DECREMENT:
      return state - 1
    default:
      return state // need to log "unrecognized action"
  }
}

// minimal implementation with no concurrency guarantee (instantaneous update assumption)
const createStore = ( reducer ) => {
  let state
  let subscribers = []

  const getState = () => state

  const dispatch = ( action ) => {
    state = reducer( state, action )
    subscribers.forEach( ( subscriber ) => subscriber() )
  }

  const subscribe = ( subscriber ) => {
    subscribers.push( subscriber )
    return () => {
      subscribers = subscribers.filter( ( _subscriber ) => _subscriber !== subscriber )
    }
  }

  return { getState, dispatch, subscribe }
}

const createAction = ( type ) => ( { type } )

// test
// const store = createStore( counter );

// store.subscribe( () => { console.log( store.getState() ) } )

// store.dispatch( { type: "INCREMENT" } )
// store.dispatch( { type: "INCREMENT" } )
// store.dispatch( { type: "INCREMENT" } )
// store.dispatch( { type: "DECREMENT" } )
// store.dispatch( { type: "invalid action" } )

module.exports = { createStore, createAction, counter, COUNTER_INCREMENT, COUNTER_DECREMENT }
