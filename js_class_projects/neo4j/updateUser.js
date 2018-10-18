const { driver, session, updateUser } = require('./src/updateUserToNeo4j')

exports.handler = (event, context, callback) => {
  updateUser(event)
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
