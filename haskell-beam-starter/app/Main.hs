{-# LANGUAGE
DeriveGeneric,
GADTs,
OverloadedStrings,
FlexibleContexts,
FlexibleInstances,
TypeFamilies,
TypeApplications,
StandaloneDeriving,
TypeSynonymInstances,
MultiParamTypeClasses,
PartialTypeSignatures
#-}

module Main where

import Prelude
import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple

import Data.Text (Text)

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
instance Beamable UserT

type User = UserT Identity
deriving instance Show User
deriving instance Eq User

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Text) deriving Generic
  primaryKey = UserId . _userEmail
instance Beamable (PrimaryKey UserT)

data ShoppingCartDb f
  = ShoppingCartDb
  { _shoppingCartUsers :: f (TableEntity UserT)
  }
  deriving Generic

instance Database be ShoppingCartDb

shoppingCartDb :: DatabaseSettings be ShoppingCartDb
shoppingCartDb = defaultDbSettings

main :: IO ()
main = do
  connection <- open "db/shoppingcart1.db"
  -- runBeamSqliteDebug putStrLn connection $
  --   runInsert
  --   . insert (_shoppingCartUsers shoppingCartDb)
  --   . insertValues $
  --     [ User "james@example.com" "James" "Smith" "b4cc344d25a2efe540adbf2678e2304c"
  --     , User "betty@example.com" "Betty" "Jones" "82b054bd83ffad9b6cf8bdb98ce3cc2f"
  --     , User "sam@example.com" "Sam" "Taylor" "332532dcfaa1cbf61e2a266bd723612c"
  --     ]

  let allUsers = all_ (_shoppingCartUsers shoppingCartDb)
  runBeamSqliteDebug putStrLn connection $ do
    users <- runSelectReturningList $ select allUsers
    mapM_ (liftIO . putStrLn . show) users

  let sortUsersByFirstName =
        orderBy_
          (\user ->
            ( asc_ (_userFirstName user)
            , desc_ (_userLastName user)
            )
          )
          (all_ (_shoppingCartUsers shoppingCartDb))
  runBeamSqliteDebug putStrLn connection $ do
    users <- runSelectReturningList $ select sortUsersByFirstName
    mapM_ (liftIO . putStrLn . show) users

  let boundedQuery :: Q SqliteSelectSyntax _ _ _
      boundedQuery =
          limit_ 1
        $ offset_ 1
        $ orderBy_ (asc_ . _userFirstName)
        $ all_ (_shoppingCartUsers shoppingCartDb)
  runBeamSqliteDebug putStrLn connection $ do
    users <- runSelectReturningList (select boundedQuery :: SqlSelect SqliteSelectSyntax _)
    mapM_ (liftIO . putStrLn . show) users


