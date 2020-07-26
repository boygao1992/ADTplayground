var Phaser = require('phaser');

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

exports._add = function add(sceneContext) {
  return sceneContext.add;
}

exports._graphics = function graphics(gameObjectFactory) {
  return gameObjectFactory.graphics();
}

exports._input = function input(sceneContext) {
  return sceneContext.input;
}

