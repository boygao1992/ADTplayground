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
  var t = f(ref.value)
  ref.value = t.state
  return t.value
}

exports._write = function (record, ref) {
  ref.value = record
  return {}
}

// exports._pathRead = function (path, ref) {
//   var pointer = ref.value
//   for (var node in path) {
//     pointer = pointer[node]
//   }
//   return pointer
// }

// exports["_pathModify"] = function (path, f, ref) {
//   var pointer = ref.value
//   for (var node in path.slice(0,-1)) {
//     pointer = pointer[node]
//   }
//   var field = path.slice(-1)[0]
//   t = f(pointer[field])
//   pointer[field] = t.state
//   return t.value
// }

// exports["_pathWrite"] = function (path, val, ref) {
//   var pointer = ref.value
//   for (var node in path.slice(0,-1)) {
//     pointer = pointer[node]
//   }
//   var field = path.slice(-1)[0]
//   pointer[field] = val
//   return {}
// }
