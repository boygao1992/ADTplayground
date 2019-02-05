

# Status

- [x] GraphQLScalarType
  - [x] GraphQLInt
  - [x] GraphQLFloat
  - [x] GraphQLString
  - [x] GraphQLBoolean
  - [x] GraphQLID

- [x] GraphQLInputObjectType
  - [x] Scalar
  - [x] Record
  - [x] Newtype of Record
  - [x] List of Scalar
  - [x] List of Record
  - [x] List of Newtype
  - [x] Maybe Scalar
  - [x] Maybe Record
  - [x] Maybe Newtype of Record
  - TODO Union (not part of the spec)
    - `../purescript-generics-rep_experiment`
      - module Generic.SumReadPayload (class GenericToConstructor)
  - TODO prevent empty Record declaration

- [x] GraphQLObjectType
  - [x] Nullable and Maybe conversion for `args` and `output`
    - NOTE `source` doesn't need Nullable-Maybe conversion
    - NOTE `args` needs recursive Nullable-Maybe conversion (`class NullableAndMaybeRec`)
    - NOTE `output` needs single-layer Nullable-Maybe conversion (`class NullableAndMaybe`)
    - [x] class ToScalarObjectFieldNoArg
    - [x] class ToScalarObjectFieldWithArgs
    - [x] class ToRelationalObjectFieldNoArg
    - [x] class ToRelationalObjectFieldWithArgs
  - [x] Id ans String conversion for `source`, `args` and `output`
    - [x] class FetchScalarFields (`source`, `output`)
    - [x] class ToInputObject (`args`)
  - [x] Multi-argument input to a Record of arguments conversion for resolvers
    - NOTE mkFn3
  - [x] Aff to Promise conversion
  - [x] `target` type can be `Maybe`
    - [x] class ParseList
      - add a case for (Maybe target)
    - [x] class ToDeps
      - Unit -> Nullable (GraphQLType (Maybe target))
    - [x] class ToRelationalObjectFieldNoArg
      - Unit -> Nullable (GraphQLType (Maybe target))
      - instance toRelationalObjectFieldNoArgImpl
        - (Nullable (GraphQLType (Maybe target)))
    - [x] class ToRelationalObjectFieldWithArgs
      - Unit -> Nullable (GraphQLType (Maybe target))
      - instance toRelationalObjectFieldWithArgsImpl
        - (Nullable (GraphQLType (Maybe target)))
    - [x] class ToRelationalObjectFieldHandleDepListDispatch
      - instance toRelationalObjectFieldHandleDepListDispatchBaseCase
        - NOTE split into two cases
        - Bool.False (Maybe target) target (Maybe dep)
          - unsafeCoerce (depFn unit)
        - Bool.False target target target
          - nonNull (unsafeCoerce (depFn unit))

- [x] RootGraphQLObjectType
  - [x] recursively collect all reachable entities from root node
    - [x] class CollectEntities
  - [x] collect semi-completed GraphQLType constructors
    - [x] infer constructor types
      - class CollectEntityDependencies
  - [x] inject dependencies
    - [x] circular reference
      - `../purescript_record_ref`
        - Record.ST.Nested (peekLazyRef)
        - Main (main), plan
      - NOTE need to hold an extra type variable for `Region` throughout the entire process
      - [x] construct a mutable object Record for all entities
        - [x] prepare a null instance for each entity
          - Nullable (GraphQLType (Maybe spec))
      - [x] construct its dependency Record
        - a Record of lazyRef for all entities from the object Record
        - Record.ST.Nested (peekLazyRef)
          - Unit -> Nullable (GraphQLType (Maybe spec))
      - [x] mutate the object Record with the dependency Record
        - [x] prepare the corresponding subset of the dependency Record for each constructor
        - [x] execute each constructor with the subset of the dependency Record
        - Control.Monad.ST.Ref (modify)
      - [x] seal the object Record to be immutable
        - ST.run
  - ToObject without `source`

- [ ] Context

- [ ] Description

# Usage

the complete example is in `/src/Examples/ForumExample` and `/test/Main.purs`

## 1. Declare Domain Entities and Relations

```purescript
newtype User = User
    -- Scalar fields
  { id :: Id
  , name :: String
    -- Relations towards other entities
  , posts :: { date :: String } -> Array Post
             -- vvvvvvvvvvvvvvvv Arguments
  , comments :: { limit :: Int } -> Array Comment
                                       -- ^^^^^^^ Target entity
  }

derive instance genericUser :: Generic User _ -- require a dervied Generic instance to fetch its constructor name

newtype Post = Post
  { id :: Id
  , author :: User
  , comments :: { limit :: Int } -> Array Comment
  }

derive instance genericPost :: Generic Post _

newtype Comment = Comment
  { id :: Id
  , author :: User
  , post :: Post
  }

derive instance genericComment :: Generic Comment _
```

## 2. Declare Root Query

```purescript
newtype Query = Query
  { user :: { id :: Id } -> Maybe User
  , post :: { id :: Id } -> Maybe Post
  , comment :: { id :: Id } -> Maybe Comment
  , posts :: { id :: Id, limit :: Int } -> Array Post
  , comments :: { id :: Id, limit :: Int } -> Array Comment
  }

derive instance genericQuery :: Generic Query _
```

## 3. Build Entity Constructors

### 3.1 provide entity type to `toObject`
```purescript
postConstructor = toObject (Proxy :: Proxy Post)
```

### 3.2 let the compiler derive the types of required resolvers

```purescript
postConstructor :: 
  -- Resolvers for each field
  { author :: 
    { source :: { id :: String}} 
    -> Aff { id :: String }
  , comments :: 
    { source :: { id :: String}
    , args :: { limit :: Int}
    }
    -> Aff (Array { id :: String})
  , id :: 
    Maybe
    ( { source :: { id :: String}}
      -> Aff String
    )
  }
  ->  { "Comment" :: Unit -> Nullable (GraphQLType (Maybe Comment))
      , "User" :: Unit -> Nullable (GraphQLType (Maybe User))
      }
  -> GraphQLType (Maybe Post)
postConstructor = toObject (Proxy :: Proxy Post)
```

### 3.3 complete resolvers following the types

```purescript
authorResolver ::
    { source :: { id :: String}} 
    -> Aff { id :: String }
authorResolver { source: { id } } = ...

commentsResolver :: 
    { source :: { id :: String}
    , args :: { limit :: Int}
    }
    -> Aff (Array { id :: String})
commentsResolver { source: { id }, args: { limit } }= ...
```

### 3.4 inject resolvers into previously declared constructor

```purescript
post = 
  postConstructor
  { id: Nothing
  , author: authorResolver
  , comments: commentsResolver
  }
```
which is ready to use in the Root Query constructor

Its type signature denotes its dependencies on other entities' `GraphQLObjectType`
```purescript
post ::
  { "Comment" :: Unit -> Nullable (GraphQLType (Maybe Comment))
  , "User" :: Unit -> Nullable (GraphQLType (Maybe User))
  }
  -> GraphQLType (Maybe Post)
```

## 4. Build Root Query Object

### 4.1 provide root query type to `toRootObject`
```purescript
queryConstructor = 
  toRootObject (Proxy :: Proxy Query)
```

From its derived type signature, we can see types of all the required entity constructors:
```purescript
queryConstructor ::
  { "Comment" :: { "Post" :: Unit -> Nullable (GraphQLType (Maybe Post))   
                 , "User" :: Unit -> Nullable (GraphQLType (Maybe User))   
                 }                                                         
                 -> GraphQLType (Maybe Comment)                            
  , "Post" :: { "Comment" :: Unit -> Nullable (GraphQLType (Maybe Comment))
              , "User" :: Unit -> Nullable (GraphQLType (Maybe User))      
              }                                                            
              -> GraphQLType (Maybe Post)                                  
  , "User" :: { "Comment" :: Unit -> Nullable (GraphQLType (Maybe Comment))
              , "Post" :: Unit -> Nullable (GraphQLType (Maybe Post))      
              }                                                            
              -> GraphQLType (Maybe User)                                  
  }                                                                        
  -> ...
```

`toRootObject` will traverse the entire entity graph and gather all the entities connected to the root query.
So even if the root query is declared as following,
```purescript
newtype Query = Query
  { user :: { id :: Id } -> Maybe User
  }
```
the type signature of required entity constructors will remain the same because `User` has relational edges towards `Post` and `Comment`.

### 4.2 provide entity constructors to `toRootObject`

```purescript
import Examples.ForumExample.Constructor (comment, post, user)

queryConstructor =
  toRootObject
    (Proxy :: Proxy Query)
    { "User": user
    , "Post": post
    , "Comment": comment
    }
```

### 4.3 provide root source type to `toRootObject`

```purescript
queryConstructor =
  toRootObject
    (Proxy :: Proxy Query)
    { "User": user
    , "Post": post
    , "Comment": comment
    }
    (Proxy :: Proxy Unit) -- no root source is needed in this example so set it to Unit
```

Behind the scene, `toRootObject` will construct circular references among entities' `GraphQLObjectType` by "tying the knot" with `ST` effect (restricted/scoped mutation).

### 4.4 let the compiler derive the types of required resolvers

```purescript
queryConstructor :: 
  { comment :: 
    { source :: Unit
    , args :: { id :: String
              }
    }
    -> Aff
        ( Maybe
            { id :: String
            }
        )
  , comments :: 
    { source :: Unit
    , args :: { limit :: Int
              }
    }
    -> Aff
        ( Array
            { id :: String
            }
        )
  , post :: 
    { source :: Unit
    , args :: { id :: String
              }
    }
    -> Aff
        ( Maybe
            { id :: String
            }
        )
  , posts :: 
    { source :: Unit
    , args :: { limit :: Int
              }
    }
    -> Aff
        ( Array
          { id :: String
          }
        )
  , user :: 
    { source :: Unit
    , args :: { id :: String
              }
    }
    -> Aff
        ( Maybe
            { id :: String
            }
        )
  }
  -> ...
  -> GraphQLRootType Query Unit
queryConstructor =
  toRootObject
    (Proxy :: Proxy Query)
    { "User": user
    , "Post": post
    , "Comment": comment
    }
    (Proxy :: Proxy Unit)
```

### 4.5 complete resolvers following the derived types

```purescript
commentResolver :: forall source.
    { source :: source
    , args :: { id :: String
              }
    }
    -> Aff
        ( Maybe
            { id :: String
            }
        )
commentResolver { args: { id } } = ...

commentsResolver ...
postResolver ...
postsResolver ...
userResolver ...
```

### 4.6 inject the resolvers into previously declared root query constructor

```purescript
query :: GraphQLRootType Query Unit
query =
  queryConstructor
    { user: userResolver
    , post: postResolver
    , comment: commentResolver
    , posts: postsResolver
    , comments: commentsResolver
    }
```

## 5. Provide Root Query Object to Schema

```purescript
import Examples.ForumExample.Query (Query, query)

schema :: Schema Query Unit
schema = G.schema query
```
which is ready to use in GraphQL parser (`GraphQL.Type.Internal (graphql)`)

# Value without type annotation can be ambiguous at type-level

## Scalars are unambiguous

```purescript
1 :: Int
1.0 :: Number
"a" :: String
'a' :: Char
```

## Closed records with unambiguous values are unambiguous
```purescript
{ id: 0
, name: "wenbo"
} :: Record ( id :: Int
            , name :: String
            )
```

## Records in function arguments can be ambiguous

```purescript
(\{ id } -> id :: String
) :: forall r
   . { id :: String 
     | r
     }
  -> String
```

by default, compiler makes a loose/structural assumption about the records in arguments
- record is open with a free variable `r` declared in scope
  - to denote any other fields that may exist in the record
  - will be filled with a concrete type (potentially an empty Row, `()`) when this function is called with a concrete and closed record

```purescript
getId :: forall r. { id :: String | r } -> String
getId = \{ id } -> id :: String

getId { id: "robot000" } -- r = ()
```

