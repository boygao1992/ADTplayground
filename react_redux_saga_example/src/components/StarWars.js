import React from 'react'

class StarWars extends React.Component {
  render() {
    // input data
    const { starwars: { people } } = this.props
    // handlers
    const { fetchStarWarsRequest } = this.props
    return (
      <div>
        <h1>StarWars</h1>
        <ul>
          { people.map( ( { name } ) => (
            <li key={ name }>{ name }</li>
          )) }
        </ul>
        <button onClick={ fetchStarWarsRequest }>Load More</button>
      </div>
    )
  }
}

export default StarWars
