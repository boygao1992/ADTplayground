"use strict";

var zmq = require("zeromq");

exports.bindImpl = function bindImpl (socket,addr) {
  socket.bind(addr);
};

exports.connectImpl = function connectImpl (socket, addr) {
  socket.connect(addr);
};

exports.sendManyImpl = function sendManyImpl (socket, msgs) {
  socket.send(msgs);
};
