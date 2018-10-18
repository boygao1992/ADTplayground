const { driver, session, addUser } = require('./src/addUserToNeo4j')

exports.handler = (event, context, callback) => {
  addUser(event)
    .fork(
      err => {
        console.error(err)
        driver.close()
        session.close()
        callback({ status: 'fail' }, null)
      },
      data => {
        console.log(data)
        driver.close()
        session.close()
        callback(null, { status: 'success' })
      }
    )
}
