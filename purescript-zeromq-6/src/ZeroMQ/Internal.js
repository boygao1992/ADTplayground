"use strict";

var zmq = require("zeromq");

exports.bindImpl = function bindImpl (socket,addr) {
  return socket.bind(addr);
};

exports.closedImpl = function closedImpl (socket) {
  return socket.closed
}

exports.connectImpl = function connectImpl (socket, addr) {
  socket.connect(addr);
};

exports.disconnectImpl = function disconnectImpl (socket, addr) {
  socket.disconnect(addr);
};

exports.newPublisher = function newPublisher () {
  return new zmq.Publisher;
}

exports.newPull = function newPull () {
  return new zmq.Pull;
}

exports.newPush = function newPush () {
  return new zmq.Push;
}

exports.newSubscriber = function newSubscriber () {
  return new zmq.Subscriber;
}

exports.receiveImpl = function receiveImpl(socket) {
  return socket.receive();
}

exports.sendManyImpl = function sendManyImpl (socket, msgs) {
  return socket.send(msgs);
};

exports.subscribeImpl = function subscribeImpl (socket, topics) {
  return socket.subscribe(topics);
}

exports.subscribeAllImpl = function subscribeAllImpl (socket) {
  return socket.subscribe();
}

exports.unbindImpl = function unbindImpl (socket,addr) {
  return socket.unbind(addr);
};
