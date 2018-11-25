// @flow

import { connect } from 'react-redux'
import GemList from '../components/GemList'
import type { State, Handlers, Config } from '../components/GemList'
import type { Dispatch } from '../reducers'
import type { State as AppState } from '../reducers'
import { saveGemThunk } from '../thunks'

const mapStateToProps = ({ gems } : AppState) : State => ({ state: gems })

const config : Config = { title : "Search Results" }
const SearchResultList = GemList(config)

const mapDispatchToProps = (dispatch : Dispatch) : Handlers =>
  ({ handlers:
     { toggleSave: name => { dispatch(saveGemThunk(name)) } }
   })

const Connected =
  connect(
    mapStateToProps,
    mapDispatchToProps
  )(SearchResultList)

export default Connected
