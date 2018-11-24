// @flow

import React from 'react';
import SearchBox from './SearchBox'
import GemList from './GemList'
import { connect } from 'react-redux'

class App extends React.Component<any> {
  render() {
    return (
      <div>
        <SearchBox />
        <GemList />
      </div>
    )
  }
}

const Connected = connect()(App)

export default Connected
