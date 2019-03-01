"use strict"

var xlsx = require( "xlsx" )

exports._toCSV = function _toCSV( Left, Right ) {
  return function ( content, tableName ) {
    try {
      var parsed = xlsx.read( content, { type: "base64" } )
    } catch ( e ) {
      return Left( "Wrong content encoding: XLSX in Base64 expected." )
    }
    if ( parsed.Sheets.hasOwnProperty( tableName ) ) {
      return Right( xlsx.utils.sheet_to_csv( parsed.Sheets[ tableName ] ) )
    } else {
      return Left( "The given table name doesn't exist." )
    }
  }
}
