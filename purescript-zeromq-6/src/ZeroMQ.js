"use strict";

var zmq = require("zeromq");


exports.connectImpl = function connectImpl (socket,addr) {
  socket.connect(addr);
};

exports.sendManyImpl = function sendManyImpl (socket,msgs) {
  socket.send(msgs);
};
