module Main where

import RIO
import System.IO (openBinaryFile)

withBinaryFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withBinaryFile path mode = bracket (openBinaryFile path mode) hClose

data Parent = Parent1 Child1 | Parent2 Child2
  deriving (Show, Typeable)
instance Exception Parent

data Child1 = Child1
  deriving (Show, Typeable)
instance Exception Child1 where
  toException = toException . Parent1
  fromException se =
    case fromException se of
      Just (Parent1 c) -> Just c
      _ -> Nothing

data Child2 = Child2
  deriving (Show, Typeable)
instance Exception Child2 where
  toException = toException . Parent2
  fromException se =
    case fromException se of
      Just (Parent2 c) -> Just c
      _ -> Nothing

main1 :: IO ()
main1 = runSimpleApp do
  throwIO Child1 `catch` \(_ :: SomeException) -> logInfo "Caught it!"
  throwIO Child1 `catch` \(_ :: Parent) -> logInfo "Caught it again!"
  throwIO Child1 `catch` \(_ :: Child1) -> logInfo "One more catch!"
  throwIO Child1 `catch` \(_ :: Child2) -> logInfo "Missed!"

main2 :: IO ()
main2 = runSimpleApp do
  res1 <- try $ throwString "This will be caught"
  logInfo $ displayShow (res1 :: Either StringException ())

  res2 <- try $ pure ()
  logInfo $ displayShow (res2 :: Either StringException ())

  res3 <- try $ throwString "This will be caught as well"
  logInfo $ displayShow (res3 :: Either SomeException ())

  res4 <- try $ throwString "This will *not* be caught"
  logInfo $ displayShow (res4 :: Either IOException ())

main :: IO ()
main = runSimpleApp do
  result1 <- tryAny $ error "This will be caught"
  case result1 of
    Left e -> logInfo $ "Exception was caught: " <> displayShow e
    Right () -> logInfo "How was this successful?!?"

  result2 <- tryAny $ pure $ error "This will escape!" -- NOTE `pure` prevents `error` from being forced thus no exception thrown; `tryAny` doesn't nothing here
  case result2 of -- NOTE pattern matching forced evaluation of result2 which throws an impure exception
    Left e -> logInfo $ "Exception was caught" <> displayShow e
    Right () -> logInfo "How was this successful?!?"

  result3 <- tryAnyDeep $ pure $ error "This cannot escape anymore" -- NOTE `tryAnyDeep` forced evaluation of `pure (error ...)` by `deepseq`
  case result3 of
    Left e -> logInfo $ "Exception was caught" <> displayShow e
    Right () -> logInfo "How was this successful?!?"
