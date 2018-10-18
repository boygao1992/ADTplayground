const R = require('ramda')
const moment = require('moment')
const { inspect } = require('util')

const searchShowtimes = require('./src/showtimes_search')

const dateTimeQuery = ([gt, lt]) => ({
  range: {
    dateTime: { gt, lt }
  }
})

const esIdTimeGeoQuery = ({ size, tmsId, dateTimes, geo: { distance, lat, lon } }) =>
  ({
    size,
    _source: ['id', 'title', 'dateTime', 'theatre_name'],
    query: {
      bool: {
        must: [{
            match: {
              tmsId
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
  size: 10,
  tmsId: 'MV007327610000',
  dateTimes: [
    ['2017-12-20T20:00', '2017-12-20T23:00']
  ],
  geo: {
    distance: '3km',
    lat: 40.741895,
    lon: -73.989308
  }
}

// exports.handler = (event, context, callback) => {
  const result = R.pipe(
    esIdTimeGeoQuery,
    searchShowtimes,
    R.map(R.pipe(
      R.path(['hits', 'hits']),
      R.map(R.prop('_source')),
      R.sortBy(R.pipe(
        R.prop('dateTime'),
        time => moment(time)
        .unix()
      ))
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
