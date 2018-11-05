const path = require( 'path' )
const express = require( 'express' )
const cors = require( 'cors' )
const bodyParser = require( 'body-parser' )
const app = express()

app.use( cors() )
app.use( bodyParser.json() )

app.use( express.static( path.resolve( __dirname, 'public' ) ) )

app.post( '/api/contact', ( req, res, next ) => {
  console.log( `Received: ${JSON.stringify(req.body)}` )
  setTimeout( _ => { res.sendStatus( 200 ) }, 1000 )
  // res.json( "OK" )
} )

const PORT = 3000
const ADDRESS = `http://localhost:${PORT}`
app.listen( PORT, _ => console.log( console.log( `server is up at ${ADDRESS}` ) ) )
