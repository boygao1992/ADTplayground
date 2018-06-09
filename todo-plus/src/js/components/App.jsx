import React from 'react'
import PropTypes from 'prop-types'

import TodoList from './TodoList.jsx'

const App = ({ data }) =>
  <main>
    <TodoList todos = {data.todos}/>
  </main>

App.propTypes = {
  data: PropTypes.shape({
    todos: PropTypes.array.isRequired
  })
}

export default App
