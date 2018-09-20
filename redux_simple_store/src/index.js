// a dummy reducer
const counter = ( state = 0, action ) => {
  switch ( action.type ) {
    case "INCREMENT":
      return state + 1
    case "DECREMENT":
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

// test
const store = createStore( counter );
