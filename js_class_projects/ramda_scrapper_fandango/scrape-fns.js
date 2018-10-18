const R = require('ramda')
const { Future } = require('ramda-fantasy')
const axios = require('axios')
const cheerio = require('cheerio')

exports.loadPage = url =>
  Future((reject, resolve) =>
    axios
      .get(url, {
        headers: { accept: 'text/html' }
      })
      .then(resolve)
      .catch(reject)
  )

exports.scrapePage = selector =>
  R.pipe(cheerio.load, $ => $(selector).get())
