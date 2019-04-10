"use strict";

var XML = require( "fast-xml-parser" );
var jsonParser = new XML.j2xParser()

exports._xml2json = XML.parse;

exports._json2xml = jsonParser.parse;
