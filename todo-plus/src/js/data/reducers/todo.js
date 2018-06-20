import State from 'crocks/State'
const TOGGLE_TODO = 'TOGGLE_TODO'

export const toggleTodo = payload =>
  ({ type: TOGGLE_TODO, payload })

function todo({ type, payload }) {
  switch(type) {
  case TOGGLE_TODO:
    return State.of(payload)
  }
}

export default todo
