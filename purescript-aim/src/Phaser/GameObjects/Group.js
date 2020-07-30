"use strict";

exports._add = function add( { group, child, addToScene } ) {
  group.add( child, addToScene );
}

exports._addMultiple = function addMultiple( { group, children, addToScene } ) {
  group.addMultiple( children, addToScene );
}
