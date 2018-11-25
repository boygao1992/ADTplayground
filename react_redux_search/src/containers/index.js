// @flow

import React from 'react';
import SearchBox from './SearchBox'
import SearchResultList from './SearchResultList'
import SavedList from './SavedList'
import { connect } from 'react-redux'

class App extends React.Component<any> {
  render() {
    return (
      <div>
        <SavedList />
        <SearchBox />
        <SearchResultList />
      </div>
    )
  }
}

const Connected = connect()(App)

export default Connected
