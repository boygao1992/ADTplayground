module Run (run) where

import Import
-- import DB
import Magento.Database

import Tonatona.Beam.MySQL.Run (runBeamMySQLDebug -- , runBeamMySQLDebugSafe
                               )
import Database.Beam.Query

run :: RIO Resources ()
run = do
  logInfo "We're inside the application!"

  result <- runBeamMySQLDebug
    $ runSelectReturningList
    $ select do
        eav_attribute <- limit_ 10 $ all_ (magentoDb ^. catalogProductEntityInt)
        pure eav_attribute

  logInfo $ displayShow result


  -- runBeamMySQLDebugSafe do
  --   runInsert
  --     $ insert (persistentDb ^. persons)
  --     $ insertValues
  --       [ Person 1 "Sam" 10
  --       ]

  --   runInsert
  --     $ insert (persistentDb ^. persons)
  --     $ insertExpressions
  --       [ Person default_ (val_ "Melanie Hill") (val_ 29)
  --       ]

  -- runBeamMySQLDebugSafe
  --   $ runInsert
  --   $ insert (persistentDb ^. follows)
  --   $ insertExpressions
  --     [ Follow default_ (PersonId 23) (PersonId 1)
  --     , Follow default_ (PersonId 24) (PersonId 1)
  --     ]

  -- followers <- runBeamMySQLDebug
  --   $ runSelectReturningList
  --   $ select do
  --       user <- all_ (persistentDb ^. persons)
  --       follow <- all_ (persistentDb ^. follows)
  --       follower <- all_ (persistentDb ^. persons)
  --       guard_ (_followFollowed follow `references_` user)
  --       guard_ (_followFollower follow `references_` follower)
  --       pure (user ^. personName, follower ^. personName)

  -- logInfo $ displayShow followers
