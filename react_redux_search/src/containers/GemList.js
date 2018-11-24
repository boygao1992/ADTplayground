// @flow

import { connect } from 'react-redux'
import { actionCreators } from '../reducers'
import GemList from '../components/GemList'
import type { State, Handlers } from '../components/GemList'
import type { Dispatch } from '../reducers'
import type { State as AppState } from '../reducers'

// import React from 'react'
// type Props = State & Handlers & ({ dispatch : Dispatch })
// class GemListContainer extends React.Component<Props> {
//   componentDidMount() {
//   }
//   render() {
//     const { state, handlers } = this.props
//     return (
//       <GemList state={state} handlers={handlers}/>
//     )
//   }
// }
//

const { toggleSave } = actionCreators

const mapStateToProps = ({ gems } : AppState) : State => ({ state: gems })

const mapDispatchToProps = (dispatch : Dispatch) : Handlers =>
  ({ handlers:
     { toggleSave: name => { dispatch(toggleSave(name)) } }
   })

const Connected =
  connect(
    mapStateToProps,
    mapDispatchToProps
  )(GemList)

export default Connected
