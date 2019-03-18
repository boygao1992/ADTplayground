import React from 'react'

import Table from './components/table/Table.jsx';
import Select from './components/select/Select.jsx'
import { randomData } from "./Utils";

export default class App extends React.Component {
  defaultState = {
    table: { data: [] },
    select: { data: {} }
  }

  constructor() {
    super()
    this.state = this.defaultState
  }

  componentDidMount () {
    this.setState(state =>
      ({ ...state,
        table: { data: randomData(10) },
        select: { data: window.mocked_data }
      })
    )
  }

  render() {
    return (
      <div>
        <Table data={this.state.table.data} />
        <Select data={this.state.select.data}/>
      </div>
    )
  }
}
