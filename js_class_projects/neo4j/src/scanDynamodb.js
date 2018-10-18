const R = require('ramda')
const Future = require('fluture')

const AWS = require('aws-sdk');
AWS.config.loadFromPath('./awscrediential.json')

const docClient = new AWS.DynamoDB.DocumentClient();
const { DYNAMODB_TABLENAME } = require('./config')

const scanAsync = params => Future((reject, resolve) => {
  docClient.scan(params, (err, data) => {
    if (err) { reject(err) } else { resolve(data) }
  })
})

module.exports = scanAsync({ TableName: DYNAMODB_TABLENAME })
