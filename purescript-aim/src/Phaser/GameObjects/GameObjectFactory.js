"use strict";

exports._arc = function arc( { gameObjectFactory, x, y, radius, startAngle, endAngle, anticlockwise, fillColor, fillAlpha } ) {
  return gameObjectFactory.arc( x, y, radius, startAngle, endAngle, anticlockwise, fillAlpha );
}

exports._circle = function circle( { gameObjectFactory, x, y, radius, fillColor, fillAlpha } ) {
  return gameObjectFactory.circle( x, y, radius, fillColor, fillAlpha );
}

exports._container = function container( { gameObjectFactory, x, y, children } ) {
  return gameObjectFactory.container( x, y, children );
}

exports._group = function group( { gameObjectFactory, children } ) {
  return gameObjectFactory.group( children );
}

exports._line = function ( { gameObjectFactory, x, y, x1, y1, x2, y2, strokeColor, strokeAlpha } ) {
  return gameObjectFactory.line( x, y, x1, y1, x2, y2, strokeColor, strokeAlpha );
}

exports._rectangle = function rectangle( { gameObjectFactory, x, y, width, height, fillColor, fillAlpha } ) {
  return gameObjectFactory.rectangle( x, y, width, height, fillColor, fillAlpha );
}
