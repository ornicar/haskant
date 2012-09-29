module Main where

import Data.List
import Data.Maybe (mapMaybe)
-- import System.IO

import Ants

-- | Picks the first "passable" order in a list
-- returns Nothing if no such order exists
tryOrder :: World -> [Order] -> Maybe Order
tryOrder w = find (passable w)

-- | Generates orders for an Ant in all directions
generateOrders :: Ant -> [Order]
generateOrders a = map (Order a) [North .. West]

{- | 
 - Implement this function to create orders.
 - It uses the IO Monad so algorithms can call timeRemaining.
 -
 - GameParams data holds values that are constant throughout the game
 - GameState holds data that changes between each turn
 - for each see Ants module for more information
 -}
doTurn :: GameParams -> GameState -> [Order]
doTurn _ gs = mapMaybe (tryOrder (world gs)) generatedOrders 
  where generatedOrders = map generateOrders $ myAnts $ ants gs 

-- | This runs the game
main :: IO ()
main = game doTurn
