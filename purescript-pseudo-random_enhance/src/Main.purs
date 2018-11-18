module Main where

import Prelude

import Effect (Effect)
import Effect.Console (logShow) as Console
import Random.PseudoRandom (randomSeed)
import Utils (getClosestEventsWithCheapestTicket)
import GenData (genData)


main :: Effect Unit
main = do
  seed <- randomSeed
  let initState = genData seed
  Console.logShow
    $ getClosestEventsWithCheapestTicket
        5
        { x : 0.0, y : 0.0 }
        initState.events
        initState.tickets
