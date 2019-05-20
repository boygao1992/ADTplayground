{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImpredicativeTypes #-}

module DB where

import RIO

import Database.Beam.Schema

-----------
-- Database

data PersistentDb f = PersistentDb
  { _persistentPersons :: f (TableEntity PersonT)
  , _persistentFollows :: f (TableEntity FollowT)
  , _persistentBlogPosts :: f (TableEntity BlogPostT)
  } deriving (Generic, Database be)

persistentDb :: DatabaseSettings be PersistentDb
persistentDb =
  defaultDbSettings `withDbModification`
    dbModification
    { _persistentPersons =
        setEntityName "person"
        <> modifyTableFields tableModification
            { _personId = "id"
            , _personName = "name"
            , _personAge = "age"
            }
    , _persistentFollows =
        setEntityName "follow"
        <> modifyTableFields tableModification
            { _followId = "id"
            , _followFollower = PersonId "follower"
            , _followFollowed = PersonId "followed"
            }
    , _persistentBlogPosts =
        setEntityName "blog_post"
        <> modifyTableFields tableModification
            { _blogpostId = "id"
            , _blogpostTitle = "title"
            , _blogpostAuthorId = PersonId "authorId"
            }
    }

PersistentDb
  (TableLens persons)
  (TableLens follows)
  (TableLens blogposts)
  = dbLenses

---------
-- Person

data PersonT f = Person
  { _personId :: Columnar f Word64
  , _personName :: Columnar f Text
  , _personAge :: Columnar f Int
  } deriving (Generic, Beamable)
type Person = PersonT Identity
deriving instance Eq Person
deriving instance Show Person

instance Table PersonT where
   data PrimaryKey PersonT f = PersonId (Columnar f Word64)
     deriving (Generic, Beamable)
   primaryKey = PersonId . _personId
type PersonId = PrimaryKey PersonT Identity
deriving instance Eq PersonId
deriving instance Show PersonId

Person
  (LensFor personId)
  (LensFor personName)
  (LensFor personAge)
  = tableLenses

---------
-- Follow

data FollowT f = Follow
  { _followId :: Columnar f Word64
  , _followFollower :: PrimaryKey PersonT f
  , _followFollowed :: PrimaryKey PersonT f
  } deriving (Generic, Beamable)
type Follow = FollowT Identity
deriving instance Eq Follow
deriving instance Show Follow

instance Table FollowT where
  data PrimaryKey FollowT f = FollowId (Columnar f Word64)
    deriving (Generic, Beamable)
  primaryKey = FollowId . _followId
type FollowId = PrimaryKey FollowT Identity
deriving instance Eq FollowId
deriving instance Show FollowId

Follow
  (LensFor followId)
  (PersonId (LensFor followFollower))
  (PersonId (LensFor followFollowed))
  = tableLenses

-----------
-- BlogPost

data BlogPostT f = BlogPost
  { _blogpostId :: Columnar f Word64
  , _blogpostTitle :: Columnar f Text
  , _blogpostAuthorId :: PrimaryKey PersonT f
  } deriving (Generic, Beamable)
type BlogPost = BlogPostT Identity
deriving instance Eq BlogPost
deriving instance Show BlogPost

instance Table BlogPostT where
  data PrimaryKey BlogPostT f = BlogPostId (Columnar f Word64)
    deriving (Generic, Beamable)
  primaryKey = BlogPostId . _blogpostId

type BlogPostId = PrimaryKey BlogPostT Identity
deriving instance Eq BlogPostId
deriving instance Show BlogPostId

BlogPost
  (LensFor blogpostId)
  (LensFor blogpostTitle)
  (PersonId (LensFor blogpostAuthorId))
  = tableLenses
