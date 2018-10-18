const R = require('ramda')
const fs = require('fs-extra')
const path = require('path')

const data = fs.readJsonSync(path.resolve(__dirname, '../data/showtimes.json'))

const indexFormat = num => Number(num).toLocaleString('arab', {minimumIntegerDigits: 5, useGrouping: false})

const result = R.pipe(
  // List of movies
  R.chain(R.pipe(
    // Atom of movie
    movie => {
      const { tmsId, title, genres, showtimes } = movie
      return R.addIndex(R.map)(
        (showtime, index) => {
          const { dateTime, location, theatre_name, theatre_id } = showtime
          return {
            id: tmsId + indexFormat(index),
            tmsId,
            title,
            genres,
            dateTime,
            location,
            theatre_id,
            theatre_name
          }
        }
      )(showtimes)
    }
  ))
)(data)

fs.outputJsonSync(path.resolve(__dirname, '../data/showtimes_es.json'), result)
console.log('success')
