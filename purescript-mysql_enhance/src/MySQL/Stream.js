"use strict";

var mysql = require( "mysql" );

exports[ "_queryStream" ] = function ( opts, values, conn, streamOpts ) {
  return conn.query( opts, values )
    .stream( streamOpts );
}
