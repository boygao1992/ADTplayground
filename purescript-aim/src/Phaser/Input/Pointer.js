"use strict";

exports._getInterpolatedPosition = function getInterpolatedPosition ( { pointer, steps } ) {
  return pointer.getInterpolatedPosition( steps );
}

exports._point = function point( pointer ) {
  return Object.freeze( { x: pointer.x, y: pointer.y } );
}
