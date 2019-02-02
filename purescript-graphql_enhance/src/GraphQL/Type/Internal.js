"use strict"

const G = require("graphql")

// NOTE testing
// exports.int = "GraphQLInt"
exports.int = G.GraphQLInt

// NOTE testing
// exports.float = "GraphQLFloat"
exports.float = G.GraphQLFloat

// NOTE testing
// exports.string = "GraphQLString"
exports.string = G.GraphQLString

// NOTE testing
// exports.id = "GraphQLID"
exports.id = G.GraphQLID

// NOTE testing
// exports.boolean = "GraphQLBoolean"
exports.boolean = G.GraphQLBoolean

exports._nonNull = function (type) {
  // NOTE testing
  // return { "nonNull": type }

  return new G.GraphQLNonNull(type)
}

exports._list = function (type) {
  // NOTE testing
  // return { "GraphQLList": type }

  return new G.GraphQLList(type)
}

// { name :: String, description :: Maybe String, fields :: Record fieldsRow }
exports._objectType = function (objectRecord) {
  // NOTE testing
  // return { "GraphQLObjectType": objectRecord }
  return new G.GraphQLObjectType(objectRecord)
}

// exports._objectTypeField = function (type, args, resolve, description) {
//   return { type, args, resolve, description }
// }

exports._inputObjectType = function (inputObjectRecord) {
  // NOTE testing
  // return  { "GraphQLInputObjectType": inputObjectRecord }

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

exports._schema = function(query) {
  return new G.GraphQLSchema({
    query: query
  })
}

// NOTE from purescript-graphql
exports._graphql = function(
  schema,
  requestString,
  rootValue,
  contextValue,
  variableValues,
  operationName
) {
  return function(onError, onSuccess) {
    G.graphql(
      schema,
      requestString,
      rootValue,
      contextValue,
      variableValues,
      operationName
    )
    .then(onSuccess)
    .catch(onError)

    return function(cancelError, cancelerError, cancelerSuccess) {
      cancelerError(new Error("Cancellation is not supported yet."));
    }
  }
}
