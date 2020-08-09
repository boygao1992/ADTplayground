"use strict";

exports._clear = function clear ( renderTexture ) {
  renderTexture.clear();
}

exports._draw = function draw ( { renderTexture, entries, x, y, alpha } ) {
  return renderTexture.draw( entries, x, y, alpha );
}
