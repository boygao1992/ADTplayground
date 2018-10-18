const { Future } = require('ramda-fantasy')
const request = require('request')
const R = require('ramda')
const {
  /* algebraic operators */
  map,
  chain,
  /* helper functions */
  // Function composition
  pipe,
  applySpec,
  // Map
  prop,
  // List
  head,
  tail
} = R
const moment = require('moment')
const path = require('path')
const fs = require('fs-extra')
const { inspect } = require('util')
const get_theatre_address = require('./get_theatre_address')

const loadData = url => Future((reject, resolve) => {
  request(url, (error, response, body) => {
    resolve(JSON.parse(body))
  })
})
// const loadData = url => Future((reject, resolve) => {
//   fs.readJson(path.resolve(__dirname, '../data/showings'), (err, data) => {
//     if (err) { reject(err) } else { resolve(data) }
//   })
// })

const date = moment()
  .format('YYYY-MM-DD')
const numDays = 30
const radius = 100
const zipcode = 10025
const api_key = 's8utjwjzpjtkmpua5aqerfw2'
const url = `http://data.tmsapi.com/v1.1/movies/showings?startDate=${date}&numDays=${numDays}&zip=${zipcode}&radius=${radius}&api_key=${api_key}`

const theatres = fs.readJsonSync(path.resolve(__dirname, '../data/theatres_curated.json'))
const theatres_ids = R.map(R.prop('id'), theatres)

const result = pipe(
  loadData,
  // Future
  map(pipe(
    // List of movies
    R.filter(R.has('genres')),
    map(pipe(
      movie => {
        const { tmsId, title, genres, showtimes } = movie
        return R.pipe(
          R.filter(showtime => R.contains(R.path(['theatre', 'id'], showtime), theatres_ids)),
          R.map(showtime => {
            const { dateTime, ticketURI } = showtime
            const { location, name, id } = R.find(R.propEq('id', showtime.theatre.id), theatres)
            return { dateTime, ticketURI, location, theatre_name: name, theatre_id: id }
          }),
          showtimes => ({
            tmsId,
            title,
            genres,
            showtimes
          })
        )(showtimes)
      }
    )),
    R.filter(R.pipe(R.prop('showtimes'), R.isEmpty, R.not))
  ))
)(url)

// result.fork(console.error, data => console.log(inspect(data, false, 5)))
result.fork(console.error, data => {
  fs.outputJsonSync(path.resolve(__dirname, '../data/showtimes.json'), data)
  console.log('success')
})
