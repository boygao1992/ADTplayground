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

const getTheatres = pipe(
  chain(pipe(
    // Atom of movie
    prop('showtimes'),
    // List of showtimes
    chain(prop('theatre'))
    // Atoms of theatres
  )),
  // List of theatres
  R.uniq,
)

const result = pipe(
  loadData,
  // Future
  map(
    // List of movies
    getTheatres
  ),
  chain(pipe(
    R.traverse(Future.of, get_theatre_address),
    map(R.filter(x => x !== 'nothing'))
  ))
)(url)

// result.fork(console.error, console.log)
result.fork(console.error, data => { fs.outputJsonSync(path.resolve(__dirname, '../data/theatres.json'), data) })
