"use strict";

var Phaser = require( 'phaser' );

exports._onPointerMove = function onPointerMove( { eventEmitter, callback } ) {
  eventEmitter.on( Phaser.Input.Events.GAMEOBJECT_POINTER_MOVE, callback );
}

exports._onPointerOut = function onPointerOut( { eventEmitter, callback } ) {
  eventEmitter.on( Phaser.Input.Events.GAMEOBJECT_POINTER_OUT, callback );
}

exports._onPointerOver = function onPointerOver( { eventEmitter, callback } ) {
  eventEmitter.on( Phaser.Input.Events.GAMEOBJECT_POINTER_OVER, callback );
}
