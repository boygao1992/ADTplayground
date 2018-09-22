import React from 'react'
import { Switch } from './Switch'

const ToggleContext = React.createContext()

class Toggle extends React.Component {
  static Button = ( props ) => (
    <ToggleContext.Consumer>
      { ({ on, toggle }) => (
        <Switch
          on = { on }
          onClick = { toggle }
          { ...props }
        />
      ) }
    </ToggleContext.Consumer>
  )
  static On = ({ children }) => (
    <ToggleContext.Consumer>
      { ({ on }) => on ? children : null }
    </ToggleContext.Consumer>
  )
  static Off = ({ children }) => (
    <ToggleContext.Consumer>
      { ({ on }) => on ? null : children }
    </ToggleContext.Consumer>
  )

  toggle = () => this.setState(
    ({ on }) => ({ on : !on }),
    /* currentState => ({on: !currentState.on}),*/
    () => { this.props.onToggle(this.state.on) }
  )
  state = { on: false , toggle: this.toggle }

  render () {
    return (
      <ToggleContext.Provider value = {
        this.state
      }>
        {this.props.children}
      </ToggleContext.Provider>
    )
  }
}

function Usage(
  onToggle = (...args) => console.log('onToggle', ...args)
) {
  return (
    <Toggle onToggle = { onToggle }>
      nested compound component
      <div>
        layer 1:
        <Toggle.Off>This button is off</Toggle.Off>
        <div>
          layer 2:
            <Toggle.On>This button is on</Toggle.On>
        </div>
      </div>
      <Toggle.Button />
    </Toggle>
  )
}

export default Usage
