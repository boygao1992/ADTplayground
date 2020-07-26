var Phaser = require('phaser');

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

