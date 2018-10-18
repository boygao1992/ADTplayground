const neo4j = require('neo4j-driver')
  .v1;
const Future = require('fluture')
const R = require('ramda')
const { inspect } = require('util')

const { BOLT_ENDPOINT, USER_NAME, USER_PWD } = require('./config')
const driver = neo4j.driver(BOLT_ENDPOINT, neo4j.auth.basic(USER_NAME, USER_PWD));
const session = driver.session();

const neo4jQueryTemplateAsync = ([query_template, params]) =>
  Future((reject, resolve) => {
    session.run(query_template, params)
      .then(result => { resolve(result) })
      .catch(err => { reject(err) })
  })

const oneToManyUserDistanceLengthQuery = ([user0_id, user_ids]) => [
  `UNWIND {user_ids} as user_ids
   MATCH (n1:User { id: user_ids })
   MATCH (n2:User { id: {user0_id} })
   MATCH p = allShortestPaths((n1)-[:is_friend*0..3]-(n2))
   return p`,
  { user0_id, user_ids }
]

// TODO: if someone deletes/updates one or more relationship(s), the result needs to be recalculated.
const oneToManyUserDistanceLengthRequest = R.pipe(
  R.values,
  oneToManyUserDistanceLengthQuery,
  neo4jQueryTemplateAsync,
  // Future
  R.map(R.pipe(
    R.prop('records'),
    // List of Records
    R.map(R.pipe(
      // Atom of Record
      record => record.get('p'),
      // Atom of Path
      R.converge(
        (length, id) => ({ length, id }), [
          R.prop('length'),
          R.path(['start', 'properties', 'id'])
        ]
      )
    )),
    R.sortBy(R.prop('length'))
  ))
)

// const data = {
//   user0_id: "robot01",
//   user_ids: ["robot07", "robot02", "robot04"]
// }

// oneToManyUserDistanceLengthRequest(data)
//   .fork(console.error, data => {
//     console.log(inspect(data, false, 7))
//     session.close()
//     driver.close()
//   })

module.exports = {
  driver,
  session,
  oneToManyUserDistanceLengthRequest
}
