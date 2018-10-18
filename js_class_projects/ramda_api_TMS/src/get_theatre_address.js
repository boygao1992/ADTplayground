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
  // Logic
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
} = require('./scrape-fns')
const util = require('util')

// const theatre_name = 'amc 84th'
// const theatre_name = 'Symphony Space\'s Leonard Nimoy Thalia Theatre'
const theatre_name = 'Edgewater Multiplex Cinemas'

const remove_s = theatre_name => {
  if (theatre_name.match(/'s /)) {
    return /'s ([\S\s]+)$/.exec(theatre_name)[1]
  }
  return theatre_name
}
const remove_and = theatre_name => theatre_name.replace(/&/, '')
const to_query_str = theatre_name => theatre_name.replace(/\s/g, '+')
const to_url1 = query_str => `https://www.fandango.com/search?q=${query_str}`
const to_url2 = theatre_code => `https://www.fandango.com/maps/DrivingDirections.aspx?tid=${theatre_code}`

const crawlPage = pipe(
  loadPage,
  map(prop('data'))
)

const get_theatre_address = ({ name: theatre_name, id: theatre_id }) => pipe(
  remove_s,
  remove_and,
  to_query_str,
  to_url1,
  crawlPage,
  // Future
  map(pipe(
    when(
      pipe(
        scrapePage('.page-header'),
        find(propEq('name', 'h1')),
        prop('children'),
        find(propEq('type', 'text')),
        prop('data'),
        str => /^([^\s]+)/g.exec(str)[1],
        R.equals('NO'),
        R.not
      ),
      pipe(
        scrapePage('#theaters'),
        find(propEq('name', 'section')),
        prop('children'),
        find(propEq('name', 'ul')),
        prop('children'),
        find(propEq('name', 'li')),
        prop('children'),
        find(propEq('name', 'div')),
        prop('children'),
        find(propEq('name', 'a')),
        R.path(['attribs', 'href']),
        str => /([A-Z]{5})$/g.exec(str)[0]
      )
    ),
    when(
      pipe(
        R.length,
        R.lt(5)
      ),
      _ => 'nothing'
    )
  )),
  chain(pipe(
    when(
      pipe(
        R.equals('nothing'),
        R.not
      ),
      pipe(
        to_url2,
        crawlPage,
        // Future
        map(pipe(
          str => {
            const match1 = /latLon = '([0-9.-]+), ([0-9.-]+)'/g.exec(str)
            const match2 = /theaterAddress = '([^']+)'/g.exec(str)
            const address = match2[1]
            const zipcode = /(\d+)$/g.exec(address)[1]
            return {
              id: theatre_id,
              name: theatre_name,
              location: {
                lat: match1[1],
                lon: match1[2],
              },
              address,
              zipcode
            }
          }
        ))
      )
    ),
    when(
      R.equals('nothing'),
      _ => Future.of('nothing')
    )
  ))
)(theatre_name)

module.exports = get_theatre_address
// get_theatre_address(theatre_name).fork(console.error, console.log)
