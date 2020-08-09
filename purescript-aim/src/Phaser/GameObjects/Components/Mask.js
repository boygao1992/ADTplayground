"use strict";

exports._createGeometryMask = function createGeometryMask( graphics ) {
  return graphics.createGeometryMask();
}

exports._setMask = function setMask( { gameObject, mask } ) {
  return gameObject.setMask(mask);
}
