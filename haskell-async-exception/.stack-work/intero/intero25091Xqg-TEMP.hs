module Main1 where

import Prelude
import Control.Exception

catchAny :: forall a. IO a -> (SomeException -> IO a) -> IO a
catchAny = catch

dangerous :: IO Int
dangerous = error "Fool you!"

main :: IO ()
main = do
  result <- catchAny dangerous $ \e -> do
    putStrLn $ "Got an exception: " ++ show e
    putStrLn "Returning dummy value of -1"
    pure (-1)
  print result

{-
> Got an exception: Fool you!
> CallStack (from HasCallStack):
>   error, called at src/Main.hs:10:13 in main:Main
> Returning dummy value of -1
> -1
-}
