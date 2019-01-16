exports.unsafePathPeek = function (path, ref) {
  return function () {
    var pointer = ref
    path.forEach(function (node) {
      pointer = pointer[node]
    })
    return pointer
  }
}

exports.unsafePeekLazyRef = function (l, ref) {
  return function () {
    return function () {
      return ref[l]
    }
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

exports.unsafePathPeekLazyRef = function (path, ref) {
  return function () {
    var pointer = ref
    path.forEach(function (node) {
      pointer = pointer[node]
    })
    return function () {
      return pointer
    }
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
