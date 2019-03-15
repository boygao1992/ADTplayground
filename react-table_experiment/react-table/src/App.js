import React from "react";
import { render } from "react-dom";
import { randomData, Tips } from "./Utils";

// Import React Table
import ReactTable from "react-table";
import "react-table/react-table.css";

const makeDefaultState = () => ({
  sorted: [],
  page: 0,
  pageSize: 10,
  expanded: {},
  resized: [],
  filtered: [],
  data: []
});

export default class App extends React.Component {
  constructor() {
    super();
    this.state = makeDefaultState();
    this.resetState = this.resetState.bind(this);
  }

  componentDidMount() {
    this.setState(state => ({...state, data: randomData(10) }))
  }

  resetState() {
    this.setState(makeDefaultState());
  }

  render() {
    return (
      <div>
        <ReactTable
          data={this.state.data}
          columns={[
            {
              Header: "Id",
              accessor: "id"
            },
            {
              Header: "Make",
              accessor: "make",
            },
            {
              Header: "Model",
              accessor: "model"
            },
            {
              Header: "Year",
              accessor: "year"
            },
            {
              Header: "Trim",
              accessor: "trim"
            }
          ]}
          filterable
          defaultPageSize={10}
          className="-striped -highlight"
          // Controlled props
          sorted={this.state.sorted}
          page={this.state.page}
          pageSize={this.state.pageSize}
          expanded={this.state.expanded}
          resized={this.state.resized}
          filtered={this.state.filtered}
          // Callbacks
          onSortedChange={sorted => this.setState({ sorted })}
          onPageChange={page => this.setState({ page })}
          onPageSizeChange={(pageSize, page) =>
            this.setState({ page, pageSize })}
          onExpandedChange={expanded => this.setState({ expanded })}
          onResizedChange={resized => this.setState({ resized })}
          onFilteredChange={filtered => this.setState({ filtered })}
        />
        <br />
        <Tips />
      </div>
    );
  }
}
