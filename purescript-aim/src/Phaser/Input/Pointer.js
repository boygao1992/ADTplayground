"use strict";

exports._getInterpolatedPosition = function getInterpolatedPosition ( { pointer, steps } ) {
  return pointer.getInterpolatedPosition( steps );
}

exports._position = function position( pointer ) {
  return pointer.position;
}

exports._prevPosition = function prevPosition( pointer ) {
  return pointer.prevPosition;
}
