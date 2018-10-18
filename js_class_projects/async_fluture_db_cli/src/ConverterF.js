const converter = require('json-2-csv')
const json2csv = converter.json2csv
const csv2json = converter.csv2json
const Future = require('fluture')

// wrap json-2-csv.json2csv in a Future
const Json2CsvF = (json, opt) => Future.node( done => json2csv(json, (err, str) => { done(err, str) }))

// wrap json-2-csv.csv2json in a Future
const Csv2JsonF = (csv, opt) => Future.node( done => csv2json(csv, (err, arr) => { done(err, arr) }))

const ConverterF = {Json2CsvF, Csv2JsonF}

module.exports = ConverterF
