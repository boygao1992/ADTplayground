const R = require('ramda')
const moment = require('moment')
const { inspect } = require('util')

const searchUsers = require('./src/users_search')

const esGenreTimeGeoQuery = ({ size, genres, dateTime, geo: { distance, lat, lon } }) =>
  ({
    size,
    // _source: ['id', 'user_name', 'first_name', 'last_name', 'quizscore'],
    query: {
      bool: {
        must: [{
            terms: {
              genres
            }
          },
          {
            bool: {
              filter: {
                geo_distance: {
                  distance,
                  location: { lat, lon }
                }
              }
            }
          }
        ]
      }
    }
  })

const event = {
  size: 10,
  genres: ['action'],
  dateTime: '2017-12-20T22:00',
  geo: {
    distance: '30km',
    lat: 40.741895,
    lon: -73.989308
  }
}

const result = R.pipe(
  esGenreTimeGeoQuery,
  searchUsers,
  R.map(R.pipe(
    R.path(['hits', 'hits']),
    R.map(R.prop('_source')),
  ))
)(event)

result
  .fork(console.error, data => { console.log(inspect(data, false, 5)) })
