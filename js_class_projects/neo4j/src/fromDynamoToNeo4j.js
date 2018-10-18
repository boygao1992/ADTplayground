const R = require('ramda')
const { driver, session, putUserGraph } = require('./putUserGraph')
const scanDynamodbFuture = require('./scanDynamodb')

const result = R.pipe(
  // Future
  R.chain(R.pipe(
    R.prop('Items'),
    // List of Users
    R.filter(R.has('friendslist')),
    putUserGraph
  ))
)(scanDynamodbFuture)

result.fork(console.error, data => {
  console.log(data)
  driver.close()
  session.close()
})
