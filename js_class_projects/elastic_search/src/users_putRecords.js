const fs = require('fs-extra')
const path = require('path')
const R = require('ramda')
const Future = require('fluture')
// const { Future } = require('ramda-fantasy')

// create an elasticsearch client for your Amazon ES
const { USERS_ES_ENDPOINTS, USERS_INDEX, USERS_TYPE } = require('./def.js')
let es = require('elasticsearch')
  .Client({
    hosts: USERS_ES_ENDPOINTS,
    connectionClass: require('http-aws-es')
  })

const AWS = require('aws-sdk')
AWS.config.loadFromPath(path.resolve(__dirname, './config.json'))


const putUserRecord = user => {
    es.create({
      index: USERS_INDEX,
      type: USERS_TYPE,
      id: user.id,
      body: user,
      requestTimeout: 30 * 60 * 1000
    }, (err, res) => {
      if (err) {
        console.log(err)
      } else {
        console.log(res)
      }
    })
  }


const users = [
  {
    id: '001',
    user_name: 'boygao1992',
    first_name: 'Wenbo',
    last_name: 'Gao',
    genres: ['action', 'comedy'],
    availability: [
      {
        gte: '2017-12-20T20:00',
        lte: '2017-12-20T23:00'
      }
    ],
    location: {
      lat: 40.78,
      lon:-73.97
    },
    zipcode: 10025,
    quizscore: 99
  }
]

users.forEach(putUserRecord)
