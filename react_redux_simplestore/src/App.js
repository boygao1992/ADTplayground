import React, { Component } from 'react'
import logo from './logo.svg'
import './App.css'
import { createStore, createAction, counter, COUNTER_INCREMENT, COUNTER_DECREMENT } from './redux_simple'

export const store = createStore(counter)

const Counter = ({ value = 0, onIncrement, onDecrement }) =>
  (<div>
    <h1>{value}</h1>
    <button onClick={onIncrement}>+</button>
    <button onClick={onDecrement}>-</button>
  </div>)

export class App extends Component {
  render() {
    return (
      <div className="App">
        <header className="App-header">
          <img src={logo} className="App-logo" alt="logo" />
          <h1 className="App-title">Welcome to React</h1>
        </header>
        <p className="App-intro">
          To get started, edit <code>src/App.js</code> and save to reload.
        </p>
        <div>
          <Counter
            value = { store.getState() }
            onIncrement = { () => store.dispatch(createAction(COUNTER_INCREMENT)) }
            onDecrement = { () => store.dispatch(createAction(COUNTER_DECREMENT)) }
          />
        </div>
      </div>
    )
  }
}

