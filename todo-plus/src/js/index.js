import 'babel-polyfill'
import '../less/main.less'

import React from 'react'

import {
  render
} from 'react-dom'

import App from './components/App.jsx'

const data = {
  todos: [
    {title: 'todo1', completed: true},
    {title: 'todo2'},
    {title: 'todo3'},
  ]
}

const appEl = document.getElementById('app')

render( < App data = {data} /> , appEl )
