const fs = require('fs-extra')
const path = require('path')
const R = require('ramda')
const Future = require('fluture')
// const { Future } = require('ramda-fantasy')

// create an elasticsearch client for your Amazon ES
const { SHOWTIMES_ES_ENDPOINTS, SHOWTIMES_INDEX, SHOWTIMES_TYPE } = require('./def.js')
let es = require('elasticsearch')
  .Client({
    hosts: SHOWTIMES_ES_ENDPOINTS,
    connectionClass: require('http-aws-es')
  })

const AWS = require('aws-sdk')
AWS.config.loadFromPath(path.resolve(__dirname, './config.json'))


const esIndicesCreateAsync = R.curry((index, type, id, body) =>
  Future((reject, resolve) => {
    es.create({
      index,
      type,
      id,
      body,
      requestTimeout: 10000000
    }, (err) => {
      if (err) { resolve(err) } else { resolve('success') }
    })
  })
)

const data = fs.readJsonSync(path.resolve(__dirname, '../../TMS/data/showtimes_es.json'))

data.forEach(
  showtime => {
    es.create({
      index: SHOWTIMES_INDEX,
      type: SHOWTIMES_TYPE,
      id: showtime.id,
      body: showtime,
      requestTimeout: 30 * 60 * 1000
    }, (err, res) => {
      if (err) {
        console.log(err)
      } else {
        console.log(res)
      }
    })
  }
)

// const result = R.pipe(
//   R.traverse(
//     Future.of,
//     showtime => {
//       const { id } = showtime
//       return esIndicesCreateAsync(SHOWTIMES_INDEX, SHOWTIMES_TYPE, id, showtime)
//     }
//   )
// )(data)

// result.fork(console.error, console.log)
