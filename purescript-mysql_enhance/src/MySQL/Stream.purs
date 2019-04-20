module MySQL.Stream where

import Effect.Uncurried (EffectFn4, runEffectFn4)
import MySQL.Connection (QueryOptions, Connection)
import MySQL.QueryValue (QueryValue)
import Node.FS.Stream (ReadStreamOptions)
import Node.Stream (Readable)
import Effect (Effect)

foreign import _queryStream :: EffectFn4 QueryOptions (Array QueryValue) Connection ReadStreamOptions (Readable ())

queryStream
  :: QueryOptions
  -> (Array QueryValue)
  -> Connection
  -> ReadStreamOptions
  -> Effect (Readable ())
queryStream opts values conn streamOpts
  = runEffectFn4 _queryStream opts values conn streamOpts
