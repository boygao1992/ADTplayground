"use strict";

exports._arc = function arc( { gameObjectFactory, x, y, radius, startAngle, endAngle, anticlockwise } ) {
  return gameObjectFactory.arc( x, y, radius, startAngle, endAngle, anticlockwise );
}

exports._circle = function circle( { gameObjectFactory, x, y, radius } ) {
  return gameObjectFactory.circle( x, y, radius );
}

exports._container = function container( { gameObjectFactory, x, y, children } ) {
  return gameObjectFactory.container( x, y, children );
}

exports._group = function group( { gameObjectFactory, children } ) {
  return gameObjectFactory.group( children );
}

exports._line = function ( { gameObjectFactory, x, y, x1, y1, x2, y2 } ) {
  return gameObjectFactory.line( x, y, x1, y1, x2, y2 );
}

exports._rectangle = function rectangle( { gameObjectFactory, x, y, width, height } ) {
  return gameObjectFactory.rectangle( x, y, width, height );
}

exports._renderTexture = function renderTexture( { gameObjectFactory, x, y, width, height } ) {
  return gameObjectFactory.renderTexture( x, y, width, height );
}
