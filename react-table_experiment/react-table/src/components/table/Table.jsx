import React from "react";

import Tips from "./Tips";
import ReactTable from "react-table";
import "react-table/react-table.css";

const makeDefaultState = () => ({
  sorted: [],
  page: 0,
  pageSize: 10,
  expanded: {},
  resized: [],
  filtered: []
});

export default class Table extends React.Component {
  constructor(props) {
    super(props);
    this.state = makeDefaultState();
    this.resetState = this.resetState.bind(this);
  }

  componentDidMount() {
    /* const data = window.mocked_data
     * console.log(data)
     * this.setState(state => ({...state, data })) */
  }

  resetState() {
    this.setState(makeDefaultState());
  }

  render() {
    return (
      <div>
        <ReactTable
          data={this.props.data}
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

        <Tips />
      </div>
    );
  }
}
