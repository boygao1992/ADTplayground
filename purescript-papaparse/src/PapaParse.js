"use strict";

var papa;
if ( typeof process !== 'undefined' &&
  process.versions != null &&
  process.versions.node != null ) {
  papa = require( "papaparse" )
} else {
  papa = window.Papa
}

exports[ "_parse" ] = function ( str ) {
  return papa.parse( str )
}
