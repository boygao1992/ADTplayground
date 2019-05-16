{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImpredicativeTypes #-}

module DB where

import RIO

import Database.Beam.Schema

-----------
-- Database

data PersistentDb f = PersistentDb
  { persistentPersons :: f (TableEntity PersonT)
  , persistentFollows :: f (TableEntity FollowT)
  , persistentBlogPosts :: f (TableEntity BlogPostT)
  } deriving (Generic, Database be)

persistentDb :: DatabaseSettings be PersistentDb
persistentDb =
  defaultDbSettings `withDbModification`
    dbModification
    { persistentPersons =
        setEntityName "person"
        <> modifyTableFields tableModification
            { personId = "id"
            , personName = "name"
            , personAge = "age"
            }
    , persistentFollows =
        setEntityName "follow"
        <> modifyTableFields tableModification
            { followId = "id"
            , followFollower = PersonId "follower"
            , followFollowed = PersonId "followed"
            }
    , persistentBlogPosts =
        setEntityName "blog_post"
        <> modifyTableFields tableModification
            { blogpostId = "id"
            , blogpostTitle = "title"
            , blogpostAuthorId = PersonId "authorId"
            }
    }

PersistentDb
  (TableLens _persons)
  (TableLens _follows)
  (TableLens _blogposts)
  = dbLenses

---------
-- Person

data PersonT f = Person
  { personId :: Columnar f Word64
  , personName :: Columnar f Text
  , personAge :: Columnar f Int
  } deriving (Generic, Beamable)
type Person = PersonT Identity
deriving instance Eq Person
deriving instance Show Person

instance Table PersonT where
   data PrimaryKey PersonT f = PersonId (Columnar f Word64)
     deriving (Generic, Beamable)
   primaryKey = PersonId . personId
type PersonId = PrimaryKey PersonT Identity
deriving instance Eq PersonId
deriving instance Show PersonId

-- _personName :: Functor f2 => (Columnar f1 Text -> f2 (Columnar f1 Text)) -> PersonT f1 -> f2 (PersonT f1)
-- _personAge :: Functor f2 => (Columnar f1 Int -> f2 (Columnar f1 Int)) -> PersonT f1 -> f2 (PersonT f1)

Person
  (LensFor _personId)
  (LensFor _personName)
  (LensFor _personAge)
  = tableLenses

---------
-- Follow

data FollowT f = Follow
  { followId :: Columnar f Word64
  , followFollower :: PrimaryKey PersonT f
  , followFollowed :: PrimaryKey PersonT f
  } deriving (Generic, Beamable)
type Follow = FollowT Identity
deriving instance Eq Follow
deriving instance Show Follow

instance Table FollowT where
  data PrimaryKey FollowT f = FollowId (Columnar f Word64)
    deriving (Generic, Beamable)
  primaryKey = FollowId . followId
type FollowId = PrimaryKey FollowT Identity
deriving instance Eq FollowId
deriving instance Show FollowId

Follow
  (LensFor _followId)
  (PersonId (LensFor _followFollower))
  (PersonId (LensFor _followFollowed))
  = tableLenses

-----------
-- BlogPost

data BlogPostT f = BlogPost
  { blogpostId :: Columnar f Word64
  , blogpostTitle :: Columnar f Text
  , blogpostAuthorId :: PrimaryKey PersonT f
  } deriving (Generic, Beamable)
type BlogPost = BlogPostT Identity
deriving instance Eq BlogPost
deriving instance Show BlogPost

instance Table BlogPostT where
  data PrimaryKey BlogPostT f = BlogPostId (Columnar f Word64)
    deriving (Generic, Beamable)
  primaryKey = BlogPostId . blogpostId

type BlogPostId = PrimaryKey BlogPostT Identity
deriving instance Eq BlogPostId
deriving instance Show BlogPostId

BlogPost
  (LensFor _blogpostId)
  (LensFor _blogpostTitle)
  (PersonId (LensFor _blogpostAuthorId))
  = tableLenses
