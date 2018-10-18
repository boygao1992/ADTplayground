const R = require('ramda')
const { inspect } = require('util')

const searchShowtimes = require('./src/showtimes_search')

const dateTimeQuery = ([gt, lt]) => ({
  range: {
    dateTime: { gt, lt }
  }
})

const esGenreTimeGeoQuery = ({ size, genres, dateTimes, geo: { distance, lat, lon } }) =>
  ({
    size,
    _source: ['tmsId', 'title'],
    query: {
      bool: {
        must: [{
            terms: {
              genres
            }
          },
          {
            bool: {
              should: R.map(R.pipe(
                dateTimeQuery
              ))(dateTimes)
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
  size: 500,
  genres: ['action'],
  dateTimes: [
    ['2017-12-20T08:00', '2017-12-20T23:00']
  ],
  geo: {
    distance: '3km',
    lat: 40.741895,
    lon: -73.989308
  }
}

// exports.handler = (event, context, callback) => {
  const result = R.pipe(
    esGenreTimeGeoQuery,
    searchShowtimes,
    R.map(R.pipe(
      R.path(['hits', 'hits']),
      R.map(R.prop('_source')),
      R.uniqBy(R.prop('tmsId'))
    ))
  )(event)

  result
    .fork(
      err => {
        console.error(err)
        // callback({status: 'fail'}, null)
      },
      data => {
        console.log(inspect(data, false, 5))
        // callback(null, data)
      })
// }
