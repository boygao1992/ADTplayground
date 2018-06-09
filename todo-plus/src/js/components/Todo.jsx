import React from 'react'
import PropTypes from 'prop-types'

import classnames from 'classnames'

const Todo = ({ todo }) => {
  const { title, completed } = todo
  const classes = classnames('todo', {
    'todo--completed': completed
  })
  return <li className = {classes}>{title}</li>
}

Todo.propTypes = {
  todo: PropTypes.shape({
    title: PropTypes.string,
    completed: PropTypes.bool
  })
}

export default Todo
