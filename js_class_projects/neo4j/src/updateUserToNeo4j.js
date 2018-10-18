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

/* create/update properties on each user
 MERGE (n:User {id: {map.id} })
 ON CREATE SET n = {map}
 ON MATCH SET n += {map}
 */
const deleteExistFriendshipQuery = user0_id => [
  `MATCH (:User {id: {user0_id} })-[r:is_friend]-(:User)
   DELETE r`,
  { user0_id }
]

const updateOneToManyUserEdgeQuery = ([user0_id, user_ids]) => [
  `UNWIND {user_ids} as user_ids
   MATCH (n2:User { id: {user0_id} })
   MATCH (n1:User { id: user_ids })
   MERGE (n1)-[:is_friend]-(n2)`,
  { user0_id, user_ids }
]

// TODO: need transaction
const deleteExistFriendshipRequest = R.pipe(
  deleteExistFriendshipQuery,
  neo4jQueryTemplateAsync
)
const updateOneToManyUserEdgeRequest = R.pipe(
  updateOneToManyUserEdgeQuery,
  neo4jQueryTemplateAsync
)

const updateUser = R.pipe(
  R.when(
    R.pipe(
      R.has('friendslist'),
      R.not
    ),
    R.merge(R.__, { friendslist: [] })
  ),
  user => {
    const { userId, friendslist } = user
    return deleteExistFriendshipRequest(userId)
      .chain(_ => updateOneToManyUserEdgeRequest([userId, friendslist]))
  }
)


// const pseudoData = {
//   userId: 'robot03',
//   // friendslist: ['robot00', 'robot01']
// }

// updateUser(pseudoData)
//   .fork(console.error, data => {
//     console.log(data)
//     session.close()
//     driver.close()
//   })

module.exports = {
  driver,
  session,
  updateUser
}
