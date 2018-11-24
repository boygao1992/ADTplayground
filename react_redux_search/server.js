const request = require( "request" )
const express = require( "express" )
const cors = require( "cors" )
const app = express()

if ( process.env.NODE_ENV === "production" ) {
  app.use( express.static( "dist" ) )
} else {
  app.use( cors() )
}

app.use( express.json() )

const RUBY_GEMS_URL = "https://rubygems.org"
const PORT = 3001

app.get( "/api/??/*", ( req, res ) => {
  console.log("request: " + req.originalUrl)
  const url = `${RUBY_GEMS_URL}${req.originalUrl}`

  request.get( url, ( err, response, body ) => {
    if ( response.statusCode === 400 ) {
      res.json( [] )
    } else {
      res.json( JSON.parse( body ) )
    }
  } )
} )

app.listen( PORT, _ => {
  console.log( `Ruby Gems proxy API is now running at: http://localhost:${PORT}` )
} )
