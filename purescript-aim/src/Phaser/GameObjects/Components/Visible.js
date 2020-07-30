"use strict";

exports._visible = function visible( gameObject ) {
  return gameObject.visible;
}

exports._setVisible = function setVisibile( { gameObject, value } ) {
  gameObject.setVisible( value );
}
