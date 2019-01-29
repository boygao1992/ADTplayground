'use strict';

exports._null = null;

exports._nonNull = function (value) {
  return value;
};

exports._nullable = function (onNull, onNonNull, value) {
  return value === null ? onNull : onJust(value);
};

exports._neverNull = function (value, callback) {
  return callback(null);
}
