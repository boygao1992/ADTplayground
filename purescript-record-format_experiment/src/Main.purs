module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Record.Format (format)
import Type.Data.Symbol (SProxy(..))

-- | inferred Type of formatter, given the type-level template literal containing two `FormatVar`s, `name` and `number`
-- formatter
--   :: forall a b r
--   .  FormatVar a
--   => FormatVar b
--   => { name :: a
--      , number :: b
--      | r
--      }
--   -> String
-- formatter =
--   format
--     (SProxy :: SProxy "Hi {name}! Your favorite number is {number}.")

main :: Effect Unit
main = do
  log formatted

  where
    formatted :: String
    formatted =
      format
        (SProxy :: SProxy "Hi {name}! Your favorite number is {number}.")
        { name : "Bill", number : 16 }
