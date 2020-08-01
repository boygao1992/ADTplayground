"use strict";

exports._point = function point( pointer ) {
  return Object.freeze( { x: pointer.x, y: pointer.y } );
}
