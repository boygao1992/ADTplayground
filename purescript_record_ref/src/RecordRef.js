"use strict"

exports.new = function (record) {
  return function () {
    return { value : record }
  }
}

exports.read = function (ref) {
  return function () {
    return ref.value
  }
}

exports["_modify'"] = function (f, ref) {
  return function() {
    var t = f(ref.value)
    ref.value = t.state
    return t.value
  }
}

exports._write = function (record, ref) {
  return function () {
    ref.value = record
    return {}
  }
}

exports._pathRead = function (path, ref) {
  return function () {
    var pointer = ref.value
    path.forEach(function (node) {
      pointer = pointer[node]
    })
    return pointer
  }
}

exports["_pathModify'"] = function (path, f, ref) {
  return function () {
    if (path.length === 0) {
      var t = f(ref.value)
      ref.value = t.state
      return t.value
    }
    var pointer = ref.value
    path.slice(0, -1).forEach(function (node) {
      pointer = pointer[node]
    })
    var field = path.slice(-1)[0]
    t = f(pointer[field])
    pointer[field] = t.state
    return t.value
  }
}

exports["_pathWrite"] = function (path, val, ref) {
  return function () {
    if (path.length === 0) {
      ref.value = val
      return ref
    }
    var pointer = ref.value
    path.slice(0, -1).forEach(function (node) {
      pointer = pointer[node]
    })
    var field = path.slice(-1)[0]
    pointer[field] = val
    return {}
  }
}
