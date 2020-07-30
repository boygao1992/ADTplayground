"use strict";

exports._add = function add( { container, child } ) {
  return container.add( child );
}

exports._setSize = function setSize( { container, width, height } ) {
  container.setSize( width, height );
}
