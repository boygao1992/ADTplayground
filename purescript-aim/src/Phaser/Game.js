var Phaser = require('phaser');

exports.auto = Phaser.AUTO;

exports._newGame = function newGame(config) {
  return new Phaser.Game(config);
}

