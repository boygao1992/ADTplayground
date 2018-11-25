
// @flow

import { connect } from 'react-redux'
import GemList from '../components/GemList'
import type { State
            , Handlers as GemListHandlers
            , Config
            } from '../components/GemList'
import type { Dispatch } from '../reducers'
import type { State as AppState } from '../reducers'
import { saveGemThunk, fetchSavedGemsThunk} from '../thunks'

import React from 'react'

type Handlers
  = GemListHandlers
  & ({ initSavedList : () => void })
type Props = State & Handlers

const config : Config = { title : "Saved Gems" }
const SavedList = GemList(config)

class SavedListContainer extends React.Component<Props> {
  componentDidMount() {
    this.props.initSavedList()
  }
  render() {
    const { state, handlers } = this.props
    return (
      <SavedList state={state} handlers={handlers}/>
    )
  }
}


const mapStateToProps = ({ saved } : AppState) : State => ({ state: saved })

const mapDispatchToProps = (dispatch : Dispatch) : Handlers =>
  ({ handlers:
     { toggleSave: name => { dispatch(saveGemThunk(name)) }
     }
   , initSavedList : () => { dispatch(fetchSavedGemsThunk()) }
   })

const Connected =
  connect(
    mapStateToProps,
    mapDispatchToProps
  )(SavedListContainer)

export default Connected
