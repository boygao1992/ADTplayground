"use strict"

const G = require("graphql")

exports.int = G.GraphQLInt

exports.float = G.GraphQLFloat

exports.string = G.GraphQLString

exports.id = G.GraphQLID

exports.boolean = G.GraphQLBoolean

exports.nonNull = function (type) {
  return function () {
    return new G.GraphQLNonNull(type)
  }
}

exports.list = function (type) {
  return function () {
    return new G.GraphQLList(type)
  }
}

exports._objectType = function (name, fields, description) {
  return new G.GraphQLObjectType({ name, fields, description })
}

exports._objectTypeField = function (type, args, resolve, description) {
  return { type, args, resolve, description }
}

exports._inputObjectType = function (name, fields, description) {
  return new G.GraphQLInputObjectType({ name, fields, description })
}

exports._inputObjectTypeField = function (type, description) {
  return { type, description }
}

// name :: String
// description :: Maybe String
// values :: Array { name :: String, value :: EnumValue }
exports._enumType = function (name, values, description) {
  let valueMap = {}
  for (let value of values) {
    valueMap[value.name] = value.value
  }
  return new G.GraphQLEnumType({ name, values: valueMap, description })
}

exports._enumValue = function (value, description) {
  return { value, description }
}
