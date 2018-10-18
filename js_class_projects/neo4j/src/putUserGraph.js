const neo4j = require('neo4j-driver')
  .v1;
const Future = require('fluture')
const R = require('ramda')
const { inspect } = require('util')

const { BOLT_ENDPOINT, USER_NAME, USER_PWD } = require('./config')
const driver = neo4j.driver(BOLT_ENDPOINT, neo4j.auth.basic(USER_NAME, USER_PWD));
const session = driver.session();

// const neo4jQueryAsync = query_str =>
//   Future((reject, resolve) => {
//     session.run(query_str)
//       .then(result => { resolve(result) })
//       .catch(err => { reject(err) })
//   })

const neo4jQueryTemplateAsync = ([query_template, params]) =>
  Future((reject, resolve) => {
    session.run(query_template, params)
      .then(result => { resolve(result) })
      .catch(err => { reject(err) })
  })

// const createUserNodeQuery = user_id => [
//   'MERGE (n:User { id: {user_id} })',
//   { user_id }
// ]

// const createUserToUserEdgeQuery = ([user_id1, user_id2]) => [
//   `MATCH (a:User), (b:User)
//    WHERE a.id = {user_id1} AND b.id = {user_id2}
//    MERGE (a)-[r1:is_friend]-(b)`,
//   { user_id1, user_id2 }
// ]

// const createMultiUserQuery = user_ids => [
//   `UNWIND {user_ids} as user_ids
//    MERGE (n:User { id: user_ids.id })`,
//   { user_ids }
// ]

// const createOneToManyUserEdgeQuery = ([user0_id, user_ids]) => [
//   `UNWIND {user_ids} as user_ids
//    MATCH (n1:User) WHERE n1.id = user_ids.id
//    MATCH (n2:User { id: {user0_id} })
//    MERGE (n1)-[:is_friend]-(n2)`,
//   { user0_id, user_ids }
// ]

const safeCreateOneToManyUserEdgeQuery = ([user0_id, user_ids]) => [
  `UNWIND {user_ids} as user_ids
   MERGE (n1:User { id: user_ids })
   MERGE (n2:User { id: {user0_id} })
   MERGE (n1)-[:is_friend]-(n2)`,
  { user0_id, user_ids }
]

// const createUserNodeRequest = R.pipe(
//   createUserNodeQuery,
//   neo4jQueryTemplateAsync
// )
// const createUserToUserEdgeRequest = R.pipe(
//   createUserToUserEdgeQuery,
//   neo4jQueryTemplateAsync
// )

const safeCreateOneToManyUserEdgeRequest = R.pipe(
  safeCreateOneToManyUserEdgeQuery,
  neo4jQueryTemplateAsync
)

const putUserGraph = R.traverse(
  Future.of,
  R.pipe(
    user => [user.userId, user.friendslist],
    safeCreateOneToManyUserEdgeRequest
  )
)

// const pseudoData = [
//   { id: 'robot00', friends: ['robot02', 'robot04', 'robot06', 'robot01'] },
//   { id: 'robot01', friends: ['robot03', 'robot05', 'robot07', 'robot00'] },
//   { id: 'robot08', friends: ['robot03', 'robot05', 'robot07', 'robot00'] },
// ]

// putUserGraph(pseudoData)
//   .fork(console.error, data => {
//     console.log(data)
//     session.close()
//     driver.close()
//   })

module.exports = {
  driver,
  session,
  putUserGraph
}
