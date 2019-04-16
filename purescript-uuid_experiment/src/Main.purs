module Main where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import Data.UUID (genUUID)
import Control.Safely (foldM)
import Data.List as List
import Data.List ((:))

main :: Effect Unit
main = do
  let sample = List.fromFoldable
                [ { id: 1}
                , { id: 2}
                ]

  processed <- foldM
    (\acc x -> do
        uuid <- genUUID
        pure $ ({ id: x.id, uuid: uuid }) : acc

    )
    List.Nil
    sample

  logShow processed
