
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
type Props = State & Handlers & Config

class SavedListContainer extends React.Component<Props> {
  componentDidMount() {
    this.props.initSavedList()
  }
  render() {
    const { config, state, handlers } = this.props
    return (
      <GemList config={config} state={state} handlers={handlers}/>
    )
  }
}


const mapStateToProps = ({ saved } : AppState) : State => ({ state: saved })

const config : Config = { config : { title : "Saved Gems" } }

const addConfig = (config : Config) => (state : State) =>
  ({ ...state, ...config })

const mapDispatchToProps = (dispatch : Dispatch) : Handlers =>
  ({ handlers:
     { toggleSave: name => { dispatch(saveGemThunk(name)) }
     }
   , initSavedList : () => { dispatch(fetchSavedGemsThunk()) }
   })

const Connected =
  connect(
    state => addConfig(config)(mapStateToProps(state)),
    mapDispatchToProps
  )(SavedListContainer)

export default Connected
