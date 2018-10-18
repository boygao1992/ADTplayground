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

// const theatre_name = 'amc 84th'
// const theatre_name = 'Symphony Space\'s Leonard Nimoy Thalia Theatre'
// const theatre_name = 'Edgewater Multiplex Cinemas'
const theatre_name = 'Elinor Bunin Munroe Film Ce'
// const theatre_name = 'Film Society of Lincoln Center\'s Elinor Bunin Munroe Film Ce'
const query_str = theatre_name.replace(/\s/g, '+')
const url = `https://www.fandango.com/search?q=${query_str}`

const crawlPage = pipe(
  loadPage,
  map(prop('data'))
)

const result = pipe(
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
  ))
)(url)
// result.fork(console.error, data => { console.log(util.inspect(data, false, 4)) })
result.fork(console.error, console.log)
