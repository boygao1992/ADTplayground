"use strict";

exports._setInteractive_ = function setInteractive_( gameObject ) {
  gameObject.setInteractive();
}

exports._setInteractive = function setInteractive( { gameObject, shape, callback, dropZone } ) {
  gameObject.setInteractive( shape, callback, dropZone );
}
