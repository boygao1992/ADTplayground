const R = require('ramda')
const path = require('path')
const { Json2CsvAsync, ConverterAsync } = require('./ConverterAsync')
const Promise = require('bluebird')
// const fs = Promise.promisifyAll(require('fs-extra'))
const fs = require('fs-extra')

const Future = require('fluture')
const Either = require('monet').Either
const { Left, Right } = Either
const { Json2CsvF, Csv2Json } = require('./ConverterF')

const outputFileF = (file, data) =>
      Future.node( (rej, res) =>
              fs.outputFile(file,data, (err) =>
                            err ? rej(err) : res('success'))
            )

/**
 *
 * @param {Object} x Keys and values in String, no nested structures
 * @param {Object} y Keys and values in String, no nested structures
 * @returns {Boolean}
 * @example
 *     subsetOf({a: 'b'}, {a: 'b', c: 'd'}) //=> true
 *     subsetOf({a: 'e'}, {a: 'b', c: 'd'}) //=> false
 *     subsetOf({a: 'b', c: 'd'}, {a: 'b'}) //=> false
 */
const subsetOf = R.curry((x, y) => {
  const compare = R.eqProps(R.__, x, y)
  return R.reduce((acc, key) => acc && compare(key), true, R.keys(x))
})

/**
 *
 * @param {String} table The name of the table being read.
 * @returns {Object} Database object
 */
const readFile = async (table) => {
  const filePath = path.resolve(__dirname, `../data/${table}.csv`)
  const csv = await fs.readFileAsync(filePath, 'utf-8')
  const json = await Csv2JsonAsync(csv, { delimiter: { wrap: '"' } })
  return json
}

/**
 *
 * @param {Object} db Database object
 * @param {Object} query A JSON object that contains the query statement.
 * @returns {Array Object} Matched rows.
 */
const find = (db, query) => {
  return R.filter(subsetOf(query), db)
}

/**
 *
 * @param {Object} db Database object
 * @param {Object} row The tuple being inserted into the given table.
 * @param {String} pk Primary key of the given table.
 * @param {String} table The name of the table being processed.
 * @returns {Boolean} If insert succeeds.
 * @throws {}
 */
const insert = async (db, row, pk, table) => {
  const hasMatch = R.reduceWhile((acc, item) => !acc, (acc, item) => acc || R.propEq(pk, row[pk], item), false, db)
  if (hasMatch) { throw new Error('database: Primary Key already exists. Insert failed.') }
  const json = R.append(row,db)
  const csv = await Json2CsvAsync(json, { delimiter: { wrap: '"' } })
  const filePath = path.resolve(__dirname, `../data/${table}.csv`)
  //TODO: wrap following try-catch block in Either monad to handle error
  try {
    await fs.outputFile(filePath, csv)
  } catch (err) {
    throw new Error(`database: ${err}. Insert failed.`)
  }
  return true
}

const insertF = (db, row, pk, table) => Future.do(function* () {
  const hasMatch = R.reduceWhile((acc, item) => !acc, (acc, item) => acc || R.propEq(pk, row[pk], item), false, db)
  if (hasMatch) { return Left('database: Primary Key already exists. Insert failed.') }
  const json = R.append(row,db)
  const csv = yield Json2CsvF(json, { delimiter: { wrap: '"' } })
  const filePath = path.resolve(__dirname, `../data/${table}.csv`)
  return csv.flatMap(csv => yield outputFileF(filePath, csv))
})

const hasMatch = R.reduceWhile((acc, item) => !acc, (acc, item) => acc || R.propEq(pk, row[pk], item), false, db) ?  Left('database: Primary Key already exists. Insert failed.') : Right()

const db = {
  readFile, find, insert
}

module.exports = db
