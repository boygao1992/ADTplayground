import React from 'react'
import { Switch } from './Switch'

class Toggle extends React.Component {
  static Button = ({ on, toggle, ...props }) => (
    <Switch
      on = { on }
      onClick = { toggle }
      { ...props }
    />
  )
  static On = ({ on, children }) => (on ? children : null)
  static Off = ({ on, children }) => (on ? null : children)

  state = { on: false }
  toggle = () => this.setState(
    ({ on }) => ({ on : !on }),
    /* currentState => ({on: !currentState.on}),*/
    () => { this.props.onToggle(this.state.on) }
  )

  render () {
    return React.Children.map(this.props.children, childElement => (
      React.cloneElement(childElement, {
        on: this.state.on,
        toggle: this.toggle
      })
    ))
  }
}

function Usage(
  onToggle = (...args) => console.log('onToggle', ...args)
) {
  return (
    <Toggle onToggle = { onToggle }>
      <Toggle.On>This button is on</Toggle.On>
      <Toggle.Button />
      <Toggle.Off>This button is off</Toggle.Off>
    </Toggle>
  )
}

export default Usage
