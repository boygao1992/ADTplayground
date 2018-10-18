const R = require('ramda')
const { Future } = require('ramda-fantasy')
const {
  // loadPage :: URL -> Future Error Response
  loadPage,
  // scrapePage :: Selector -> HTML -> [DOMElement]
  scrapePage
} = require('../../scrape-fns')
const fs = require('fs-extra')
const path = require('path')

const urls = ['https://buzzkenya.com/movie-trivia-questions-answers/',
  'https://buzzkenya.com/movie-trivia-questions-answers/2/',
  'https://buzzkenya.com/movie-trivia-questions-answers/3/'
]

const crawlPage = R.pipe(
  loadPage,
  R.map(R.prop('data'))
)

const result = R.pipe(
  R.traverse(Future.of, R.pipe(
    crawlPage,
    // Future
    R.map(R.pipe(
      // List of QAs
      scrapePage('.single-content'),
      R.head,
      R.prop('children'),
      R.slice(4, -2),
      R.map(
        // Atom of QA
        R.tryCatch(
          R.pipe(
            R.prop('children'),
            R.head,
            R.prop('data'),
            str => {
              const matches = /^\d+\.\s*([^\?]+\?)\s*([^\s]+[\S\s]+)$/.exec(str)
              return {
                Q: matches[1],
                A: matches[2]
              }
            }
          ),
          R.always('nothing')
        )
      ),
      R.filter(x => x !== 'nothing')
    ))
  )),
  // Future([[], [], []])
  R.map(R.flatten)
  // Future([])
)(urls)

result.fork(console.error, data => { fs.outputJsonSync(path.dirname(__filename) + '/QA.json', data) })
