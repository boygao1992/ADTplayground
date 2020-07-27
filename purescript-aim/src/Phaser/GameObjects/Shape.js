"use strict";

exports._setIsFilled = function setIsFilled( { shape, isFilled } ) {
  shape.isFilled = isFilled;
}

exports._setAngle = function setAngle( { shape, degrees } ) {
  shape.setAngle( degrees );
}

exports._setStrokeStyle = function setStrokeStyle( { shape, lineWidth, color, alpha } ) {
  shape.setStrokeStyle( lineWidth, color, alpha );
}
