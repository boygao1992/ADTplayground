"use strict";

var Phaser = require('phaser');

exports._newLine = function newLine({ startX, startY, endX, endY }) {
  return new Phaser.Geom.Line(startX, startY, endX, endY);
}

exports._newPoint = function newPoint({ x, y }) {
  return new Phaser.Geom.Point(x, y);
}

exports._pointToLine = Phaser.Geom.Intersects.PointToLine;

