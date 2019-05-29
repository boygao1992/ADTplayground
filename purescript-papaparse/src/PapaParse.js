"use strict";

var Papa;
if ( typeof process !== 'undefined' &&
  process.versions != null &&
  process.versions.node != null ) {
  Papa = require( "papaparse" )
} else {
  Papa = window.Papa
}

exports[ "_parse" ] = function ( str ) {
  return Papa.parse( str )
}
