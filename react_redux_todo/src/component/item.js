// @flow
import React from 'react'

export type Item$State =
  { text : string
  , completed : boolean
  , editing : boolean
  }

// Query ~ Action for reducer

type Item$Query$TOGGLE_COMPLETE = { type: "TOGGLE_COMPLETE" }
const toggle_todo = (): Item$Query$TOGGLE_COMPLETE =>
  ( { type: "TOGGLE_COMPLETE" } )
type Item$Query$TOGGLE_EDIT = { type: "TOGGLE_EDIT" }
const toggle_edit = (): Item$Query$TOGGLE_EDIT =>
  ( { type : "TOGGLE_EDIT" } )

type Item$Query
  = Item$Query$TOGGLE_COMPLETE
  | Item$Query$TOGGLE_EDIT

// eval ~ reducer :: State -> Query -> State
const reducer = (state : Item$State, query : Item$Query): Item$State => {
  switch (query.type) {
    case "TOGGLE_COMPLETE":
      return ({ ...state, completed: !state.completed })
    default:
      return state
  }
}

// Input ~ props
type Item$Input$Initial = { text: string }

export type Item$Input =
  { initial: Item$Input$Initial
  }

// Output ~ handlers in props

// IO

// initializer :: Input -> State
const initializer = ( { initial: { text } }: Item$Input ) : Item$State =>
  ({ text , completed: false, editing: false })


// component
class Item extends React.Component<Item$Input> {
  store = createStore(reducer, initializer(this.props))

  componentDidMount() {
    this.store.subscribe(() => { console.log(this.store.getState())})
    this.store.subscribe(this.forceUpdate.bind(this))
  }

  handleClick = (event : SyntheticMouseEvent<HTMLSpanElement>) => {
    this.store.dispatch(toggle_todo())
  }

  // render :: State -> VDOM
  render() {
    let { text, completed } = this.store.getState()
    return (
      <div>
        <span style = {{
               textDecoration: completed ? 'line-through' : 'none'
             }}
              onClick = {this.handleClick}
              >
          { text }
        </span>
      </div>
    )
  }
}

export const component = ( initial: Item$Input$Initial) =>
{
    return (<Item initial = { initial } />)
}

// simple redux implementation

function createStore
  ( reducer: ((state : Item$State, query : Item$Query) => Item$State)
  , initialState : Item$State
  )
{
  let state : Item$State = initialState
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
