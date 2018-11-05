const express = require( 'express' )
const app = express()

// Accept cross domain for local development purpose.
const cors = require( 'cors' )
app.use( cors() )

const path = require( 'path' )
const http = require( 'http' )
const Promise = require( 'bluebird' )
const R = require( 'ramda' )
const tweets = require( './public/tweets.json' )
  .map( ( tweet ) => ( { _source: { ...tweet, coordinates: { lat: tweet.coordinates[ 0 ], lon: tweet.coordinates[ 1 ] } } } ) )

app.use( express.static( path.resolve( __dirname, 'public' ) ) )

const query = keyword => ( {
  query: {
    bool: {
      must: [ {
        term: {
          text: keyword
        }
      }, ]
    }
  }
} )

const geoQuery = ( keyword, lat, lon ) => ( {
  query: {
    bool: {
      must: [ {
          term: {
            text: keyword
          }
        },
        {
          bool: {
            filter: {
              geo_distance: {
                distance: '10km',
                coordinates: {
                  lat,
                  lon
                }
              }
            }
          }
        },
      ]
    }
  }
} )


app.get( '/term/:keyword', ( req, res ) => {
  const { keyword } = req.params
  res.json( tweets.filter( ( { _source: { text } } ) => ( text.includes( keyword ) ) )
    .slice( 0, 100 ) )
} )

app.get( '/term/:keyword/lat/:lat/lon/:lon', ( req, res ) => {
  const { keyword, lat, lon } = req.params
  res.json(
    tweets.filter( ( { _source: { text } } ) => ( text.includes( keyword ) ) )
    .filter( ( { _source: { coordinates: { lat: _lat, lon: _lon } } } ) =>
      ( Math.pow( _lat - lat, 2 ) + Math.pow( _lon - lon, 2 ) ) < 0.01 )
    .slice( 0, 100 )
  )
} )

const PORT = 8081
const ADDRESS = `http://localhost:${PORT}`
http.createServer( app )
  .listen( PORT, () => { console.log( `server is up at ${ADDRESS}` ) } )
