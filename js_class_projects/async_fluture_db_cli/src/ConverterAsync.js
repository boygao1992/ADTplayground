const converter = require('json-2-csv')
const json2csv = converter.json2csv
const csv2json = converter.csv2json
const Promise = require('bluebird')

// wrap json-2-csv.json2csv in a Promise
const Json2CsvAsync = Promise.method((json, opt) => {
  return new Promise((res, rej) => {
    json2csv(json, (err, str) => {
      if (err) { rej(err) }
      res(str)
    }, opt)
  })
})

// wrap json-2-csv.csv2json in a Promise
const Csv2JsonAsync = Promise.method((csv, opt) => {
  return new Promise((res, rej) => {
    csv2json(csv, (err, arr) => {
      if (err) { rej(err) }
      res(arr)
    }, opt)
  })
})

const ConverterAsync = {Json2CsvAsync, Csv2JsonAsync}

module.exports = ConverterAsync
