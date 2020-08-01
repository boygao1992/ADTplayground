"use strict";

var Phaser = require( 'phaser' );

exports._setHitAreaRectangle = function setHitAreaRectangle( { inputPlugin, gameObject, x, y, width, height } ) {
  inputPlugin.setHitAreaRectangle( gameObject, x, y, width, height );
}
