"use strict";

exports._setAngle = function setAngle( { shape, degrees } ) {
  shape.setAngle( degrees );
}

exports._setFillStyle = function setFillStyle( { shape, color, alpha } ) {
  shape.setFillStyle( color, alpha );
}

exports._setIsFilled = function setIsFilled( { shape, isFilled } ) {
  shape.isFilled = isFilled;
}

exports._setIsStroked = function setIsStroked( { shape, isStroked } ) {
  shape.isStroked = isStroked;
}

exports._setStrokeStyle = function setStrokeStyle( { shape, lineWidth, color, alpha } ) {
  shape.setStrokeStyle( lineWidth, color, alpha );
}
