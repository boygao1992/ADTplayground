// @flow

import SearchBox from '../components/SearchBox'
import type { State, Handlers } from '../components/SearchBox'
import { connect } from 'react-redux'
import type { Dispatch } from '../reducers'
import type { State as AppState } from '../reducers'
import { fetchGemsThunk } from '../thunks'

const mapStateToProps = ({ input } : AppState) : State => ({ state : input })
const mapDispatchToProps = (dispatch : Dispatch) : Handlers =>
  ({ handlers:
     { onValueChange : search => { dispatch(fetchGemsThunk(search)) } }
   })

const Connected =
  connect(
    mapStateToProps,
    mapDispatchToProps
  )(SearchBox)

export default Connected
