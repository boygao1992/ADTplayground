var Phaser = require('phaser');

exports.auto = Phaser.AUTO;

exports._newGame = function newGame(config) {
  return new Phaser.Game(config);
}

exports._onPointerMove = function onPointerMove({ inputPlugin, callback }) {
  inputPlugin.addListener(Phaser.Input.Events.GAMEOBJECT_POINTER_MOVE, callback);
}

exports._buildScene = function buildScene({ preload, create }) {
  var scene = function () {
    Object.getPrototypeOf(scene.prototype).constructor.call(this)
  };
  scene.prototype = Object.create(Phaser.Scene.prototype);
  scene.prototype.preload = function () {
    preload(this);
  };
  scene.prototype.create = function () {
    create(this);
  };
  return scene;
}

exports._addGraphics = function addGraphics(sceneContext) {
  return sceneContext.add.graphics();
}

exports._input = function input(sceneContext) {
  return sceneContext.input;
}

exports._newLine = function newLine({ startX, startY, endX, endY }) {
  return new Phaser.Geom.Line(startX, startY, endX, endY);
}

exports._newPoint = function newPoint({ x, y }) {
  return new Phaser.Geom.Point(x, y);
}

exports._pointToLine = Phaser.Geom.Intersects.PointToLine;

exports._clear = function clear(graphics) {
  graphics.clear();
}

exports._fillPointShape = function fillPointShape({ graphics, point, size}) {
  graphics.fillPointShape(point, size);
}

exports._fillStyle = function fillStyle({ graphics, color, alpha }) {
  graphics.fillStyle(color, alpha);
}

exports._lineStyle = function lineStyle({ graphics, lineWidth, color, alpha }) {
  graphics.lineStyle(lineWidth, color, alpha);
}

exports._strokeLineShape = function strokeLineShape({ graphics, line }) {
  graphics.strokeLineShape(line);
}

