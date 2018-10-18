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

const url = 'https://www.fandango.com/theaterlistings-prn.aspx?location=99501&sdate=12-10-2017&tid=AAQFR'
// const url = 'https://www.fandango.com/theaterlistings-prn.aspx?location=11102&pn=1&sdate=12-13-2017&tid=AABQA,AABQC,AAECG'

const crawlPage = pipe(
  loadPage,
  map(prop('data'))
)

const result = pipe(
  crawlPage,
  // Future
  map(pipe(
    scrapePage('tbody'),
    // List of Tables
    map(pipe(
      // List of Rows
      prop('children'),
      filter(propEq('name', 'tr')),
      applySpec({
        theater: pipe(
          head, // 1st Row
          // Atom of theater name
          prop('children'),
          find(propEq('name', 'td')),
          prop('children'),
          find(propEq('name', 'h4')),
          prop('children'),
          head,
          prop('data'),
          str => str.replace(/^\s+/g, '')
        ),
        movies: pipe(
          tail, // rest of Rows
          // List of movies
          map(pipe(
            // List of (2)Columns
            prop('children'),
            filter(propEq('name', 'td')),
            applySpec({
              movie: pipe(
                head, // 1st Column
                // Atom of movie name
                prop('children'),
                head,
                prop('data'),
                str => str.replace(/^\s+/g, ''),
                str => str.replace(/\n/g, ''),
                str => str.replace(/\s{2}/g, '')
              ),
              showtimes: pipe(
                last, // 2nd Column
                // List of showtimes
                prop('children'),
                find(propEq('name', 'ul')),
                prop('children'),
                find(propEq('name', 'ul')),
                prop('children'),
                filter(propEq('name', 'li')),
                map(pipe(
                  // Atom of showtime
                  prop('children'),
                  head,
                  when(
                    has('children'),
                    pipe(
                      prop('children'),
                      head
                    )
                  ),
                  prop('data')
                ))
              ),
            })
          ))
        )
      })
    ))
  )),
)(url)

// result.fork(console.error, console.log)
result.fork(console.error, data => { console.log(util.inspect(data, false, 6)) })
