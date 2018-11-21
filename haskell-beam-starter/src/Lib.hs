{-# LANGUAGE
DeriveGeneric,
GADTs,
OverloadedStrings,
FlexibleContexts,
FlexibleInstances,
TypeFamilies,
TypeApplications,
StandaloneDeriving,
TypeSynonymInstances
#-}
module Lib
    ( someFunc
    , UserT(User)
    , User
    , UserId
    ) where

import Database.Beam
import Database.Beam.Schema (Columnar)
import Database.Beam.Sqlite

import Data.Text (Text)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

{- |
type family Columnar (f :: * -> *) x where ...

Equations
  Columnar Exposed x = Exposed x
  Columnar Identity x = x
  Columnar (Lenses t f) x = LensFor (t f) (Columnar f x)
  Columnar (Nullable c) x = Columnar c (Maybe x)
  Columnar f x = f x
-}

-- Tables
data UserT f
  = User
  { _userEmail :: Columnar f Text
  , _userFirstName :: Columnar f Text
  , _userLastName :: Columnar f Text
  , _userPassword :: Columnar f Text
  }
  deriving Generic

type User = UserT Identity
deriving instance Show User
deriving instance Eq User

type UserId = PrimaryKey UserT Identity

