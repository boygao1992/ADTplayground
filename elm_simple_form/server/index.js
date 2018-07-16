const express = require( 'express' )
const cors = require( 'cors' )
const bodyParser = require( 'body-parser' )
const app = express()

app.use( cors() )
app.use( bodyParser.json() )

app.post( '/api/contact', ( req, res, next ) => {
  console.log( `Received: ${JSON.stringify(req.body)}` )
  setTimeout( _ => { res.sendStatus( 200 ) }, 1000 )
  // res.json( "OK" )
} )

app.listen( 3000, _ => console.log( 'Listening on http://localhost:3000' ) )
