"use strict";

var Phaser = require( 'phaser' );

exports._onPointerMove = function onPointerMove( { inputPlugin, callback } ) {
  inputPlugin.on( Phaser.Input.Events.GAMEOBJECT_POINTER_MOVE, callback );
}

exports._point = function point( pointer ) {
  return Object.freeze( { x: pointer.x, y: pointer.y } );
}
