module MySQL.Transaction
  ( withTransaction
  , withTransaction'
  ) where

import Prelude

import Effect.Aff (Aff, attempt, message)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import MySQL.Connection (Connection)

withTransaction
  :: forall a
   . (Connection -> Aff a)
  -> Connection
  -> Aff a
withTransaction handler conn = do
  begin conn
  r <- attempt $ handler conn
  case r of
    Left e -> do
      rollback conn
      throwError e
    Right r' -> do
      commit conn
      pure r'


withTransaction'
  :: forall a
  .  (Connection -> Aff a)
  -> Connection
  -> Aff (Either String a)
withTransaction' handler conn = do
  begin conn
  r <- attempt $ handler conn
  case r of
    Left e -> do
      rollback conn
      pure <<< Left <<< message $ e
    Right r' -> do
      commit conn
      pure <<< Right $ r'



begin :: Connection -> Aff Unit
begin = fromEffectFnAff <<< _begin



commit :: Connection -> Aff Unit
commit = fromEffectFnAff <<< _commit



rollback :: Connection -> Aff Unit
rollback = fromEffectFnAff <<< _rollback



foreign import _begin :: Connection -> EffectFnAff Unit



foreign import _commit :: Connection -> EffectFnAff Unit



foreign import _rollback :: Connection -> EffectFnAff Unit
