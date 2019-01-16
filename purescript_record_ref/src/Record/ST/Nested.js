exports.unsafePathPeek = function (path, ref) {
  return function () {
    var pointer = ref
    path.forEach(function (node) {
      pointer = pointer[node]
    })
    return pointer
  }
}

exports.unsafePeekSTRef = function (l, ref) {
  return function () {
    return { value: ref[l] }
  }
}

exports.unsafePathPeekSTRecord = function (path, ref) {
  return function () {
    var pointer = ref
    path.forEach(function (node) {
      pointer = pointer[node]
    })
    return pointer
  }
}

exports.unsafePathPeekSTRef = function (path, ref) {
  return function () {
    var pointer = ref
    path.forEach(function (node) {
      pointer = pointer[node]
    })
    return { value: pointer }
  }
}

exports.unsafePathPoke = function (path, val, ref) {
  return function () {
    var pointer = ref
    path.slice(0, -1).forEach(function (node) {
      pointer = pointer[node]
    })
    var field = path.slice(-1)[0]
    pointer[field] = val
    return {}
  }
}

exports.unsafePathModify = function (path, f, ref) {
  return function () {
    var pointer = ref
    path.slice(0, -1).forEach(function (node) {
      pointer = pointer[node]
    })
    var field = path.slice(-1)[0]
    pointer[field] = f(pointer[field])
    return {}
  }
}
