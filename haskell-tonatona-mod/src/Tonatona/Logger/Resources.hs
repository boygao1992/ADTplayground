module Tonatona.Logger.Resources where

import RIO

import Tonatona.WithResource (With, withResource, hoistWithResource)
import Tonatona.Logger.Options (HasLoggerOptions, defaultLogOptions)

-- Resource Initialization

newtype LoggerLogFunc = LoggerLogFunc { loggerLogFunc :: LogFunc }
instance HasLogFunc LoggerLogFunc where
  logFuncL = lens loggerLogFunc (\_ y -> LoggerLogFunc y )

instance HasLoggerOptions options => With options LoggerLogFunc where
  withResource = hoistWithResource $ \options cont -> do
    logOptions <- defaultLogOptions options
    withLogFunc logOptions (cont . LoggerLogFunc)
