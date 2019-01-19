"use strict"

const G = require("graphql")

exports.int = G.GraphQLInt

exports.float = G.GraphQLFloat

exports.string = G.GraphQLString

exports.id = G.GraphQLID

exports.boolean = G.GraphQLBoolean

exports.nonNull = function (type) {
  return new G.GraphQLNonNull(type)
}

exports.list = function (type) {
  return new G.GraphQLList(type)
}

// { name :: String, description :: Maybe String, fields :: Record fieldsRow }
exports._objectType = function (objectRecord) {
  return new G.GraphQLObjectType(objectRecord)
}

// exports._objectTypeField = function (type, args, resolve, description) {
//   return { type, args, resolve, description }
// }

exports._inputObjectType = function (inputObjectRecord) {
  return new G.GraphQLInputObjectType(inputObjectRecord)
}

// exports._inputObjectTypeField = function (type, description) {
//   return { type, description }
// }

// name :: String
// description :: Maybe String
// values :: Array { name :: String, value :: EnumValue }
exports._enumType = function (name, values, description) {
  var valueMap = {}
  values.forEach(function (value) {
    valueMap[value.name] = value.value
  })
  return new G.GraphQLEnumType({ name: name, values: valueMap, description: description })
}

exports._enumValue = function (value, description) {
  return { value: value, description: description }
}
