function tupleTreeToJson(tupleTree, encodeJson) {
  if (tupleTree.constructor.name === 'Branch') {
    var res = tupleTree.value0.reduce(function(json, tuple) {
      json[tuple.value0] = tupleTreeToJson(tuple.value1, encodeJson);
      return json;
    }, {});
    return res;
  }
  else {
    return encodeJson ? encodeJson(tupleTree.value0) : tupleTree.value0;
  }
}

exports._toJson = function(tupleTree, encodeJson) {
  return tupleTreeToJson(tupleTree, encodeJson);
}

exports._toObject = function(tupleTree) {
  return tupleTreeToJson(tupleTree);
} 

exports._serialize = function _serialize(record, show) {
  return '{ '+ Object.keys(record).map(function(key) {
    if (record[key].constructor.name === 'Object')
      return '{ '+ key +': '+ _serialize(record[key], show) +' }';
    else
      return key +': '+ show(record[key]);
  }).join(', ') +' }';
}

exports.hObjToJson = function(encode) {
  return function toJson(obj) {
    return Object.keys(obj).reduce(function(jsonObj, key) {
      if (obj[key].constructor.name === 'Object')
        jsonObj[key] = toJson(obj[key]);
      else
        jsonObj[key] = encode.encodeJson(obj[key]);
      return jsonObj;
    }, {});
  }
}
