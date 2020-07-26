var Phaser = require('phaser');

exports._onPointerMove = function onPointerMove({ inputPlugin, callback }) {
  inputPlugin.on(Phaser.Input.Events.GAMEOBJECT_POINTER_MOVE, callback);
}
