/* global d3 Fluture*/

const { Future } = Fluture

// Constants

const margin = { top: 15, right: 0, bottom: 0, left: 10 }
const container = { width: 225, height: 300 }
const width = container.width - margin.left - margin.right
const height = container.height - margin.top - margin.bottom

// Main

const fetchData = Future( ( rej, res ) => {
  d3.json( "data/scores.json", ( data ) => {
    res( data )
  } )
} )

const main = Future.do( function* () {
  // D3Object :: Type -> Type -> Type
  // select :: forall datum. (String | ElemRef) -> D3Object ElemRef datum
  const scores = yield fetchData

  const chart = d3.select( "body" )
    .append( "div" )
    .attr( "class", "chart" )
    .append( "svg" )
    .attr( "width", container.width )
    .attr( "height", container.height )
    .append( "g" )
    .attr( "transform", `translate( ${margin.left}, ${margin.top})` )

  const bar = chart
    .selectAll( "g" )
    .data( scores ) // data :: forall rawDatum datum. D3Object ElemRef datum ~> Array rawDatum -> (rawDatum -> datum) -> D3Object ElemRef datum
    .enter()
    .append( "g" )
    .attr( "transform", ( _, i ) => ( `translate(0, ${i * 33})` ) )
    .on( "mouseover", ( d, i, elements ) => {
      d3.select( elements[ i ] )
        .select( "rect" )
        .classed( "on", true )
    } )
    .on( "mouseout", ( d, i, elements ) => {
      d3.select( elements[ i ] )
        .select( "rect" )
        .classed( "on", false )
    } )

  const rect = bar
    .append( "rect" )
    .attr( "height", 30 )
    .attr( "width", ( { score } ) => ( score ) )
    .attr( "class", "bar" )

  const text = bar
    .append( "text" )
    .text( ( { name, score } ) => ( `${name}: ${score}` ) )
    .attr( "y", "1.2em" )
} )

main.fork( console.error, console.log )
