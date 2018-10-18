const fs = require('fs-extra')
const path = require('path')
const R = require('ramda')
const { Future } = require('ramda-fantasy')

// create an elasticsearch client for your Amazon ES
const { SHOWTIMES_ES_ENDPOINTS, SHOWTIMES_INDEX, SHOWTIMES_TYPE, SHOWTIMES_SCHEMA} = require('./def')
let es = require('elasticsearch')
  .Client({
    hosts: SHOWTIMES_ES_ENDPOINTS,
    connectionClass: require('http-aws-es')
  })

const AWS = require('aws-sdk')
AWS.config.loadFromPath(path.resolve(__dirname, './config.json'))

const esIndicesCreateAsync = R.curry((index, body) =>
  Future((reject, resolve) => {
    es.indices.create({
      index,
      body
    }, (err) => {
      if (err) { reject(err) } else { resolve('success') }
    })
  })
)

esIndicesCreateAsync(SHOWTIMES_INDEX, SHOWTIMES_SCHEMA)
  .fork(console.error, console.log)
