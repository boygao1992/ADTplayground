import React from 'react'
import PropTypes from 'prop-types'

import Todo from './Todo.jsx'

const TodoList = ({ todos }) =>
  <main>
    <ul className = 'todos'>
      {
        todos.map((todo, indx) =>
          <Todo
            key = {indx}
            todo = {todo}
          />
        )
      }
    </ul>
  </main>

TodoList.propTypes = {
  todos: PropTypes.array.isRequired
}

export default TodoList
