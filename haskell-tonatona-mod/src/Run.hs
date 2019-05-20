module Run (run) where

import Import
-- import DB
import Magento.Database
import qualified Magento.Database.Eav.Attribute as EA
import qualified Magento.Database.Eav.Attribute.Option as EAO
import qualified Magento.Database.Eav.Attribute.Option.Value as EAOV

import Tonatona.Beam.MySQL.Run (runBeamMySQLDebug -- , runBeamMySQLDebugSafe
                               )
import Database.Beam.Query


run :: RIO Resources ()
run = do
  logInfo "We're inside the application!"

  let my_attribute_code = "filterable_size"
      -- my_attribute_option_value = "10\""

  result <- runBeamMySQLDebug
    $ runSelectReturningList
    $ select do
        eav_attribute_option_value <-
          filter_ (\table -> table ^. EAOV.value /=. nothing_)
          $ all_ (magentoDb ^. eavAttributeOptionValue)
        eav_attribute_option <- all_ (magentoDb ^. eavAttributeOption)
        eav_attribute <- all_ (magentoDb ^. eavAttribute)
        guard_ (EAOV._option_id eav_attribute_option_value `references_` eav_attribute_option)
        guard_ (EAO._attribute_id eav_attribute_option `references_` eav_attribute)
        guard_ (eav_attribute ^. EA.attribute_code ==. just_ my_attribute_code)

        pure $ eav_attribute_option_value^.EAOV.value

  logInfo $ displayShow $ catMaybes result


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
