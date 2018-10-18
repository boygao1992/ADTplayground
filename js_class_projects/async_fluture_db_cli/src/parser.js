'use strict'

const R = require('ramda')

// monadic RegExp builder
const Parsimmon = require('parsimmon')
// Parsimmon.alt(p1, p2, ...pn): Accepts any number of parsers, yielding the value of the first one that succeeds, backtracking in between.
// Array Parsimmon.parser -> Parsimmon.parser
const alt = Parsimmon.alt
// Parsimmon.noneOf(string): Returns a parser that looks for exactly one character NOT from string, and yields that character.
// String -> Parsimmon.parser
const noneOf = Parsimmon.noneOf
// Parsimmon.regex(regexp): Returns a parser that looks for a match to the regexp and yields the entire text matched.
// RegExp -> Parsimmon.parser
const regex = Parsimmon.regex
// Parsimmon.string(string): Returns a parser that looks for string and yields that exact value.
// String -> Parsimmon.parser
const string = Parsimmon.string

const whitespace = Parsimmon.whitespace // /\s+/
const optWhitespace = Parsimmon.optWhitespace // /\s*/
const dot = string('.'); // /./
const equal = string('=') // /=/
const singleQuote = string("'") // /'/
const doubleQuote = string('"') // /"/

const noneQuotedWords = noneOf('"\',=').atLeast(1) // /[^,=]+/
const singleQuotedWords = noneOf("'").many().trim(singleQuote) // /'[^']*'/
const doubleQuotedWords = noneOf('"').many().trim(doubleQuote) // /"[^"]*"/
const words = alt(noneQuotedWords, singleQuotedWords, doubleQuotedWords)

/**
 *
 * @function
 * @sig [String] -> Parsimmon.parser
 * @param {Array String} list
 * @returns {Parismmon.parser}
 * @example
 *     listToAltGroup(['CustomerID', 'ShipVia']) //=> Parsimmon.alt(Parsimmon.string('CustomerID'), Parsimmon.string('ShipVia'))
 */
const listToAltGroup = list => alt(...R.map(x => string(x), list))

/**
 *
 * @function
 * @sig [a] -> Boolean
 * @param {Array a} list
 * @returns {Boolean}
 * @example
 *     hasDuplicate([1,2,3,1]) //=> true
 */
const hasDuplicate = list => {
  return list.length !== R.uniq(list).length
}

/**
 *
 * @function
 * @sig [a] -> String
 * @param {Array a} list
 * @returns {String}
 * @example
 *     charsToString(['1','2','3','4']) //=> '1234'
 *     charsToString([]) // => ''
 */
const charsToString = R.reduce((acc, val) => R.concat(acc, val), '')

/**
 *
 * @function
 * @sig [a] -> Boolean
 * @param {Array a} list
 * @returns {Boolean}
 * @example
 *     nonEmptyArray([1,2,3]) //=> true
 *     nonEmptyArray([]) //=> false
 *     nonEmptyArray('1') //=> false
 */
const nonEmptyArray = list => R.type(list) === 'Array'? list.length !== 0 : false

/**
 *
 * @function
 * @sig [[[Char]]] -> [[Char]]
 * @param {Array (Array (Array Char))} list
 * @returns {Array (Array Char)}
 * @example
 *     flatten([ [['1','2','3','4']], [], [['a','b','c']] ])
 *     //=> [ ['1','2','3','4'], [], ['a','b','c'] ]
 */
const flatten = R.map(list => nonEmptyArray(list)? R.unnest(list) : list)

/**
 *
 * @function
 * @sig [[Char]] -> [String]
 * @param {Array (Array Char)} list
 * @returns {Array String}
 * @example
 *     groupFlatten([ [['1','2','3','4']], [], [['a','b','c']] ])
 *     //=> ['1234', '', 'abc']
 */
const groupFlatten = R.pipe(flatten, R.map(charsToString))

/**
 *
 * @function
 * @sig String -> Boolean
 * @param {String} item
 * @returns {Boolean}
 * @example
 *     notEmptyString('') //=> false
 *     notEmptyString('123 8923')//=> true
 *     notEmptyString([]) //=> true
 */
const notEmptyString = item => typeof item === 'string'? string.length !== 0 : true

/**
 *
 * @function
 * @sig [String] -> Boolean
 * @param {Array String} item
 * @returns {Boolean}
 * @example
 *      noteEmptyStringFilter(['123', '', '456']) //=> ['123', '456']
 */
const notEmptyStringFilter = R.filter(notEmptyString)

const findParser = (header, str) => {
  const keyAltGroup = listToAltGroup(header)
  const conditionKey = keyAltGroup
        .trim(optWhitespace)
        .skip(equal)
        .skip(optWhitespace)
        .skip(words)
        .skip(optWhitespace)
  const conditionValue = keyAltGroup
        .trim(optWhitespace)
        .skip(equal)
        .skip(optWhitespace)
        .then(words)
        .skip(optWhitespace)
  const conditionsKeys = conditionKey
        .times(1)
        .sepBy(string(','))
  const conditionsValues = conditionValue
        .times(1)
        .sepBy(string(','))
  const conditionsKeysResult = conditionsKeys.parse(str) //=> { status: Boolean, value: Array<Array<Array<Char>>>}
  if (!conditionsKeysResult.status) { throw new Error('parser: invalid input')}
  const keys = R.pipe(groupFlatten, notEmptyStringFilter)(conditionsKeysResult.value)
  if (hasDuplicate(keys)) { throw new Error('parser: duplicate keys') }
  const values = R.pipe(groupFlatten, notEmptyStringFilter)(conditionsValues.parse(str).value)
  return R.zipObj(keys, values)
}

const insertParser = (header, str) => {
  const conditionsValues = optWhitespace
        .then(words.atMost(1))
        .sepBy(string(','))
  const conditionsValuesResult = conditionsValues.parse(str)
  if (!conditionsValuesResult.status) { throw new Error('parser: invalid input')}
  const values = groupFlatten(conditionsValuesResult.value)
  switch(true) {
    case (values.length > header.length):
      throw new Error('parser: too many arguments')
      break
    case (values.length < header.length):
      throw new Error('parser: insufficient arguments')
      break
    default:
  }
  return R.zipObj(header, values)
}

exports.findParser = findParser
exports.insertParser = insertParser
