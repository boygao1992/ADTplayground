const SHOWTIMES_INDEX = 'showtimes'
const SHOWTIMES_TYPE = 'showtime'
const SHOWTIMES_SCHEMA = {
  mappings: {
    [SHOWTIMES_TYPE]: {
      properties: {
        id: { type: 'keyword' },
        title: { type: 'text' },
        genres: { type: 'text' },
        dateTime: { type: 'date', format: 'date_hour_minute' },
        location: { type: 'geo_point' },
        theatre_id: { type: 'keyword' },
        theatre_name: { type: 'text' }
      }
    }
  }
}
const SHOWTIMES_ES_ENDPOINTS = ['https://search-movie-2baskgwz2fm6kl6booa2arygia.us-east-1.es.amazonaws.com']

const USERS_INDEX = 'users'
const USERS_TYPE = 'user'
const USERS_SCHEMA = {
  mappings: {
    [USERS_TYPE]: {
      properties: {
        id: { type: 'keyword' },
        user_name: { type: 'keyword' },
        first_name: { type: 'keyword' },
        last_name: { type: 'keyword' },
        genres: { type: 'text' },
        availability: { type: 'date_range', format: 'date_hour_minute' },
        location: { type: 'geo_point' },
        zipcode: { type: 'keyword' },
        quizscore: { type: 'integer' }
      }
    }
  }
}

const USERS_ES_ENDPOINTS = ['https://search-movie-2baskgwz2fm6kl6booa2arygia.us-east-1.es.amazonaws.com']

module.exports = {
  SHOWTIMES_INDEX,
  SHOWTIMES_TYPE,
  SHOWTIMES_SCHEMA,
  SHOWTIMES_ES_ENDPOINTS,
  USERS_INDEX,
  USERS_TYPE,
  USERS_SCHEMA,
  USERS_ES_ENDPOINTS
}
