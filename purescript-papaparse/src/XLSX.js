"use strict";

var xlsx;
if ( typeof process !== 'undefined' &&
  process.versions != null &&
  process.versions.node != null ) {
  xlsx = require( "xlsx" )
} else {
  xlsx = window.XLSX
}

exports._toCSV = function _toCSV( Left, Right, Tuple ) {
  return function ( content ) {
    try {
      var parsed = xlsx.read( content, { type: "base64" } )
    } catch ( e ) {
      return Left( "Unrecognized encoding: xlsx in Base64 expected." )
    }
    // if ( parsed.Sheets.hasOwnProperty( tableName ) ) {
    //   return Right( xlsx.utils.sheet_to_csv( parsed.Sheets[ tableName ] ) )
    // } else {
    //   return Left( "The given table name doesn't exist." )
    // }
    var sheets = []
    for ( var sheetName in parsed.Sheets ) {
      sheets.push(
        Tuple(
          sheetName,
          xlsx.utils.sheet_to_csv( parsed.Sheets[ sheetName ] )
        )
      )
    }
    return Right( sheets )
  }
}
