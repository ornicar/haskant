module Ai (
  doTurn
) where

import Data.List
import Data.Maybe (mapMaybe)

import Ants

-- Entry point
doTurn :: GameParams -> GameState -> [Order]
doTurn _ gs = mapMaybe (tryOrder (world gs)) generatedOrders 
  where generatedOrders = map generateOrders $ myAnts $ ants gs 

-- | Picks the first "passable" order in a list
-- returns Nothing if no such order exists
tryOrder :: World -> [Order] -> Maybe Order
tryOrder w = find (passable w)

-- | Generates orders for an Ant in all directions
generateOrders :: Ant -> [Order]
generateOrders a = map (Order a) [North .. West]
