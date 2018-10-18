const R = require('ramda')
const {
  /* algebraic operations */
  map,
  chain,
  traverse,
  /* helper functions */
  // Function
  pipe,
  applySpec,
  // List
  take,
  slice, // take from 'fromIndex' to 'toIndex'
  head, // take(1)
  last, // takeLast(1)
  init, // drop last
  tail, // drop head
  filter,
  find, // pipe(filter, head)
  // Map
  has,
  prop,
  propEq,
  where,
  // Control Flow
  when,
  ifElse,
  // Error Handling
  tryCatch,
  always
} = R
const { Future } = require('ramda-fantasy')
const {
  // loadPage :: URL -> Future Error Response
  loadPage,
  // scrapePage :: Selector -> HTML -> [DOMElement]
  scrapePage
} = require('../../scrape-fns')
const util = require('util')

const crawlPage = pipe(
  loadPage,
  map(prop('data'))
)

const theatre_code = 'AABQA'
const url = `https://www.fandango.com/maps/DrivingDirections.aspx?tid=${theatre_code}`

const result = pipe(
  crawlPage,
  // Future
  map(pipe(
    str => {
      const match1 = /latLon = '([0-9.-]+), ([0-9.-]+)'/g.exec(str)
      const match2 = /theaterAddress = '([^']+)'/g.exec(str)
      const address = match2[1]
      const zipcode = /(\d+)$/g.exec(address)[1]
      return {
        lat: match1[1],
        lon: match1[2],
        address,
        zipcode
      }
    }
  ))
)(url)

result.fork(console.error, console.log)
