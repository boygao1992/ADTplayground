"use strict";

var Parser = require( "fast-xml-parser" );

var json2xmlOptions = {
  supressEmptyNode: true
};
var jsonParser = new Parser.j2xParser( json2xmlOptions );

exports._xml2json = Parser.parse;

exports._json2xml = function ( json ) {
  return jsonParser.parse( json );
}
